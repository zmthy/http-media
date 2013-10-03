------------------------------------------------------------------------------
-- | A framework for parsing Accept headers and choosing the correct media
-- type.
module Network.HTTP.Accept
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
    , matches
    , Quality

      -- * Parsing
    , parseAccept

      -- * Accept matching
    , matchAccept
    , mapAccept

      -- * Content matching
    , matchContent
    , mapContent
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS

------------------------------------------------------------------------------
import Control.Applicative  (pure, (<*>), (<|>))
import Control.Monad        (guard)
import Data.ByteString      (ByteString, split)
import Data.ByteString.UTF8 (toString)

------------------------------------------------------------------------------
import Network.HTTP.Accept.Match     as Match
import Network.HTTP.Accept.MediaType as MediaType
import Network.HTTP.Accept.Quality
import Network.HTTP.Accept.Utils


------------------------------------------------------------------------------
-- | Parses a full Accept header into a list of quality-valued media types.
parseAccept :: ByteString -> Maybe [Quality MediaType]
parseAccept = (. split comma) . mapM $ \bs ->
        let (accept, q) = BS.breakSubstring ";q=" $ BS.filter (/= space) bs
        in (<*> parse accept) $ if BS.null q
            then pure maxQuality else fmap (flip Quality) $ readQ
                (toString $ BS.takeWhile (/= semi) $ BS.drop 3 q)


------------------------------------------------------------------------------
-- | Matches a list of server-side resource options against a quality-marked
-- list of client-side preferences. A result of 'Nothing' means that nothing
-- matched (which should indicate a 406 error). If two or more results arise
-- with the same quality level and specificity, then the first one in the
-- server list is chosen.
--
-- The use of the 'Match' type class allows the application of either
-- 'MediaType' for the standard Accept header or 'ByteString' for any other
-- Accept header which can be marked with a quality value. The standard
-- application of this function for 'MediaType' should be in conjunction with
-- 'parseAccepts'.
--
-- > parseAccepts header >>= matchQuality resourceTypeOptions
--
-- For more information on the matching process see RFC 2616, section 14.
matchAccept
    :: Match a
    => [a]          -- ^ The server-side options
    -> [Quality a]  -- ^ The client-side preferences
    -> Maybe a
matchAccept server clientq = guard (hq /= 0) >> specific qs
  where
    merge (Quality c q) = map (`Quality` q) $ filter (`matches` c) server
    matched = concatMap merge clientq
    (hq, qs) = foldr qfold (0, []) matched
    qfold (Quality v q) (mq, vs) = case compare q mq of
        GT -> (q, [v])
        EQ -> (mq, v : vs)
        LT -> (mq, vs)
    specific (a : ms) = Just $ foldl mostSpecific a ms
    specific []       = Nothing


------------------------------------------------------------------------------
-- | The equivalent of 'matchAccept' above, except the resulting choice is
-- mapped to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > maybe render406Error renderResource $ parseAccepts header >>= mapQuality
-- >     [ ("text/html",        asHtml)
-- >     , ("application/json", asJson)
-- >     ]
mapAccept
    :: Match a
    => [(a, b)]     -- ^ The map of server-side preferences to values
    -> [Quality a]  -- ^ The client-side preferences
    -> Maybe b
mapAccept s c = matchAccept (map fst s) c >>= lookupMatches s


------------------------------------------------------------------------------
-- | Matches a list of server-side parsing options against a the client-side
-- content value. A result of 'Nothing' means that nothing matched (which
-- should indicate a 415 error).
--
-- As with the Accept parsing, he use of the 'Match' type class allows the
-- application of either 'MediaType' or 'ByteString'.
matchContent
    :: Match a
    => a         -- ^ The client's request value
    -> [a]       -- ^ The server-side response options
    -> Maybe a
matchContent client = foldl choose Nothing
  where choose m server = m <|> (guard (matches client server) >> Just server)


------------------------------------------------------------------------------
-- | The equivalent of 'matchContent' above, except the resulting choice is
-- mapped to another value.
mapContent
    :: Match a
    => a         -- ^ The client request's header value
    -> [(a, b)]  -- ^ The map of server-side responses
    -> Maybe b
mapContent c s = matchContent c (map fst s) >>= lookupMatches s


------------------------------------------------------------------------------
-- | The equivalent of 'lookupBy matches'.
lookupMatches :: Match a => [(a, b)] -> a -> Maybe b
lookupMatches ((k, v) : r) a
    | Match.matches k a = Just v
    | otherwise         = lookupMatches r a
lookupMatches [] _ = Nothing

