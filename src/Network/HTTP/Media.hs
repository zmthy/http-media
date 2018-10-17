{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- | A framework for parsing HTTP media type headers.
module Network.HTTP.Media
    (
    -- * Self-describing media
      Media (Media, accept, media, Accept)

    -- * Media types
    , MediaType
    , (//)
    , (/:)
    , mainType
    , subType
    , parameters
    , (/?)
    , (/.)

    -- * Languages
    , Language
    , toParts

    -- * Accept matching
    , matchAccept

    -- * Content matching
    , matchContent

    -- * Quality values
    , Quality
    , quality
    , QualityOrder
    , qualityOrder
    , maxQuality
    , minQuality
    , parseQuality
    , matchQuality

    -- * Accept
    , Accept (..)

    -- * Rendering
    , RenderHeader (..)
    ) where

#if MIN_VERSION_base(4, 8, 0)
import           Control.Applicative             ((<|>))
#else
import           Control.Applicative             (pure, (<$>), (<*>), (<|>))
#endif

import qualified Data.ByteString.Char8           as BS

import           Control.Monad                   (guard, (>=>))
import           Data.ByteString                 (ByteString)
import           Data.Foldable                   (foldl', maximumBy)
import           Data.Function                   (on)
import           Data.Maybe                      (fromMaybe)
import           Data.Proxy                      (Proxy (Proxy))

import           Network.HTTP.Media.Accept       as Accept
import           Network.HTTP.Media.Language     as Language
import           Network.HTTP.Media.MediaType    as MediaType
import           Network.HTTP.Media.Quality
import           Network.HTTP.Media.RenderHeader
import           Network.HTTP.Media.Utils        (trimBS)


------------------------------------------------------------------------------
-- | Matches a list of server-side resource options against a quality-marked
-- list of client-side preferences. A result of 'Nothing' means that nothing
-- matched (which should indicate a 406 error). If two or more results arise
-- with the same quality level and specificity, then the first one in the
-- server list is chosen.
--
-- The use of the 'Accept' type class allows the application of either
-- 'MediaType' for the standard Accept header or 'ByteString' for any other
-- Accept header which can be marked with a quality value.
--
-- > matchAccept ["text/html", "application/json"] <$> getHeader
--
-- For more information on the matching process see RFC 2616, section 14.1-4.
matchAccept
    :: Accept a
    => [a]         -- ^ The server-side options
    -> ByteString  -- ^ The client-side header value
    -> Maybe a
matchAccept = (parseQuality >=>) . matchQuality



data Media a b
    = Accept { accept :: a }
    | Media
    { accept :: a
    , media  :: b
    } deriving (Eq, Ord, Show)


instance Accept a => Accept (Media a b) where
    parseAccept = fmap Accept . parseAccept
    matches = matches `on` accept
    moreSpecificThan = moreSpecificThan `on` accept


------------------------------------------------------------------------------
-- | Matches a list of server-side parsing options against a the client-side
-- content value. A result of 'Nothing' means that nothing matched (which
-- should indicate a 415 error).
--
-- > matchContent ["application/json", "text/plain"] <$> getContentType
--
-- For more information on the matching process see RFC 2616, section 14.17.
matchContent
    :: Accept a
    => [a]         -- ^ The server-side response options
    -> ByteString  -- ^ The client's request value
    -> Maybe a
matchContent options ctype = foldl choose Nothing options
  where
    choose m server = m <|> do
        parseAccept ctype >>= guard . (`matches` server)
        Just server


------------------------------------------------------------------------------
-- | Parses a full Accept header into a list of quality-valued media types.
parseQuality :: Accept a => ByteString -> Maybe [Quality a]
parseQuality = parseQuality' Proxy

parseQuality' :: Accept a => Proxy a -> ByteString -> Maybe [Quality a]
parseQuality' p = (. map trimBS . BS.split ',') . mapM $ \ s ->
    let (acc, q) = fromMaybe (s, Nothing) $ if ext then findQ s else getQ s
    in maybe (pure maxQuality) (fmap (flip Quality) . readQ) q <*>
        parseAccept acc
  where
    ext = hasExtensionParameters p

    -- Split on ';', and check if a quality value is there. A value of Nothing
    -- indicates there was no parameter, whereas a value of Nothing in the
    -- pair indicates the parameter was not a quality value.
    getQ s = let (a, b) = trimBS <$> BS.breakEnd (== ';') s in
        if BS.null a then Nothing else Just (BS.init a,
            if BS.isPrefixOf "q=" b then Just (BS.drop 2 b) else Nothing)

    -- Trawl backwards through the string, ignoring extension parameters.
    findQ s = do
        let q = getQ s
        (a, m) <- q
        maybe (findQ a) (const q) m


------------------------------------------------------------------------------
-- | Matches a list of server-side resource options against a pre-parsed
-- quality-marked list of client-side preferences. A result of 'Nothing' means
-- that nothing matched (which should indicate a 406 error). If two or more
-- results arise with the same quality level and specificity, then the first
-- one in the server list is chosen.
--
-- The use of the 'Accept' type class allows the application of either
-- 'MediaType' for the standard Accept header or 'ByteString' for any other
-- Accept header which can be marked with a quality value.
--
-- > matchQuality ["text/html", "application/json"] <$> parseQuality header
--
-- For more information on the matching process see RFC 2616, section 14.1-4.
matchQuality
    :: Accept a
    => [a]          -- ^ The server-side options
    -> [Quality a]  -- ^ The pre-parsed client-side header value
    -> Maybe a
matchQuality options acceptq = do
    guard $ not (null options)
    Quality m q <- maximumBy (compare `on` fmap qualityOrder) optionsq
    guard $ q /= 0
    return m
  where
    optionsq = reverse $ map addQuality options
    addQuality opt = withQValue opt <$> foldl' (mfold opt) Nothing acceptq
    withQValue opt qv = qv { qualityData = opt }
    mfold opt cur acq@(Quality acd _)
        | opt `matches` acd = mostSpecific acq <$> cur <|> Just acq
        | otherwise         = cur
