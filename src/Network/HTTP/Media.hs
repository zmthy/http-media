{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- | A framework for parsing HTTP media type headers.
module Network.HTTP.Media
    (
    -- * Media types
      MediaType
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
    , mapAccept
    , mapAcceptMedia
    , mapAcceptLanguage
    , mapAcceptBytes

    -- * Content matching
    , matchContent
    , mapContent
    , mapContentMedia
    , mapContentLanguage

    -- * Quality values
    , Quality
    , parseQuality
    , matchQuality
    , mapQuality

    -- * Accept
    , Accept (..)

    -- * Rendering
    , RenderHeader (..)
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS

------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 8, 0)
import Control.Applicative  ((<|>))
#else
import Control.Applicative  (pure, (<$>), (<*>), (<|>))
#endif
import Control.Monad        (guard, (>=>))
import Data.ByteString      (ByteString)
import Data.Maybe           (fromMaybe)

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept       as Accept
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Media.Language     as Language
import Network.HTTP.Media.MediaType    as MediaType
import Network.HTTP.Media.Quality
import Network.HTTP.Media.Utils        (trimBS)


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


------------------------------------------------------------------------------
-- | The equivalent of 'matchAccept' above, except the resulting choice is
-- mapped to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > getHeader >>= maybe render406Error renderResource . mapAccept
-- >     [ ("text" // "html",        asHtml)
-- >     , ("application" // "json", asJson)
-- >     ]
mapAccept
    :: Accept a
    => [(a, b)]    -- ^ The map of server-side preferences to values
    -> ByteString  -- ^ The client-side header value
    -> Maybe b
mapAccept = (parseQuality >=>) . mapQuality


------------------------------------------------------------------------------
-- | A specialisation of 'mapAccept' that only takes 'MediaType' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getHeader >>= maybe render406Error renderResource . mapAcceptMedia
-- >     [ ("text/html",        asHtml)
-- >     , ("application/json", asJson)
-- >     ]
mapAcceptMedia ::
    [(MediaType, b)]  -- ^ The map of server-side preferences to values
    -> ByteString     -- ^ The client-side header value
    -> Maybe b
mapAcceptMedia = mapAccept


------------------------------------------------------------------------------
-- | A specialisation of 'mapAccept' that only takes 'Language' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getHeader >>= maybe render406Error renderResource . mapAcceptLanguage
-- >     [ ("text/html",        asHtml)
-- >     , ("application/json", asJson)
-- >     ]
mapAcceptLanguage ::
    [(Language, b)]  -- ^ The map of server-side preferences to values
    -> ByteString    -- ^ The client-side header value
    -> Maybe b
mapAcceptLanguage = mapAccept


------------------------------------------------------------------------------
-- | A specialisation of 'mapAccept' that only takes 'ByteString' as its
-- input, to avoid ambiguous-type errors when using string literal
-- overloading.
--
-- > getHeader >>= maybe render406Error encodeResourceWith . mapAcceptBytes
-- >     [ ("compress", compress)
-- >     , ("gzip",     gzip)
-- >     ]
mapAcceptBytes ::
    [(ByteString, b)]  -- ^ The map of server-side preferences to values
    -> ByteString      -- ^ The client-side header value
    -> Maybe b
mapAcceptBytes = mapAccept


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
-- | The equivalent of 'matchContent' above, except the resulting choice is
-- mapped to another value.
--
-- > getContentType >>= maybe send415Error readRequestBodyWith . mapContent
-- >     [ ("application" // "json", parseJson)
-- >     , ("text" // "plain",       parseText)
-- >     ]
mapContent
    :: Accept a
    => [(a, b)]    -- ^ The map of server-side responses
    -> ByteString  -- ^ The client request's header value
    -> Maybe b
mapContent options ctype =
    matchContent (map fst options) ctype >>= lookupMatches options


------------------------------------------------------------------------------
-- | A specialisation of 'mapContent' that only takes 'MediaType' as its
-- input, to avoid ambiguous-type errors when using string literal
-- overloading.
--
-- > getContentType >>=
-- >     maybe send415Error readRequestBodyWith . mapContentMedia
-- >         [ ("application/json", parseJson)
-- >         , ("text/plain",       parseText)
-- >         ]
mapContentMedia
    :: [(MediaType, b)]  -- ^ The map of server-side responses
    -> ByteString        -- ^ The client request's header value
    -> Maybe b
mapContentMedia = mapContent


------------------------------------------------------------------------------
-- | A specialisation of 'mapContent' that only takes 'Language' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getContentType >>=
-- >     maybe send415Error readRequestBodyWith . mapContentLanguage
-- >         [ ("application/json", parseJson)
-- >         , ("text/plain",       parseText)
-- >         ]
mapContentLanguage
    :: [(Language, b)]  -- ^ The map of server-side responses
    -> ByteString        -- ^ The client request's header value
    -> Maybe b
mapContentLanguage = mapContent


------------------------------------------------------------------------------
-- | Parses a full Accept header into a list of quality-valued media types.
parseQuality :: Accept a => ByteString -> Maybe [Quality a]
parseQuality = parseQuality' Proxy

parseQuality' :: Accept a => Proxy a -> ByteString -> Maybe [Quality a]
parseQuality' p = (. map trimBS . BS.split ',') . mapM $ \ s ->
    let (accept, q) = fromMaybe (s, Nothing) $ if ext then findQ s else getQ s
    in maybe (pure maxQuality) (fmap (flip Quality) . readQ) q <*>
        parseAccept accept
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
    let merge (Quality c q) = map (`Quality` q) $ filter (`matches` c) options
        matched = concatMap merge acceptq
        (hq, qs) = foldr qfold (0, []) matched
        qfold (Quality v q) (mq, vs) = case compare q mq of
            GT -> (q, [v])
            EQ -> (mq, v : vs)
            LT -> (mq, vs)
        specific (a : ms) = Just $ foldl mostSpecific a ms
        specific []       = Nothing
    guard (hq /= 0)
    specific qs


------------------------------------------------------------------------------
-- | The equivalent of 'matchQuality' above, except the resulting choice is
-- mapped to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > parseQuality header >>= maybe render406Error renderResource . mapQuality
-- >     [ ("text" // "html",        asHtml)
-- >     , ("application" // "json", asJson)
-- >     ]
mapQuality
    :: Accept a
    => [(a, b)]     -- ^ The map of server-side preferences to values
    -> [Quality a]  -- ^ The client-side header value
    -> Maybe b
mapQuality options accept =
    matchQuality (map fst options) accept >>= lookupMatches options


------------------------------------------------------------------------------
-- | The equivalent of 'lookupBy matches'.
lookupMatches :: Accept a => [(a, b)] -> a -> Maybe b
lookupMatches ((k, v) : r) a
    | Accept.matches k a = Just v
    | otherwise         = lookupMatches r a
lookupMatches [] _ = Nothing
