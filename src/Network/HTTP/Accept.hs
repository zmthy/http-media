{-# LANGUAGE OverloadedStrings #-}

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
    , MediaType.matches

      -- * Quality values
    , Quality
    , (/!)
    , quality

      -- * Parsing
    , parseAccepts

      -- * Matching
    , match
    , matchQ1
    , mapMatch
    , mapMatchQ1
    ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<*>))
import           Control.Monad (liftM)

import           Data.ByteString (ByteString, split)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)
import           Data.Maybe (listToMaybe)

------------------------------------------------------------------------------
import           Network.HTTP.Accept.Match as Match
import           Network.HTTP.Accept.MediaType as MediaType
import           Network.HTTP.Accept.Quality
import           Network.HTTP.Accept.Utils


------------------------------------------------------------------------------
-- (Renamed just to keep a standard start character for operators).
-- | Constructs a Quality.
(/!) :: a -> Float -> Quality a
(/!) = (:!)


------------------------------------------------------------------------------
-- | Parses a full Accept header into a list of quality-valued media types.
parseAccepts :: ByteString -> Maybe [Quality MediaType]
parseAccepts = mapM parseAccept . split comma

parseAccept :: ByteString -> Maybe (Quality MediaType)
parseAccept bs = (<*> parse accept) $ if BS.null q
        then return (:! 1)
        else liftM (flip (:!)) $ safeRead
            (toString $ BS.takeWhile (/= semi) $ BS.drop 3 q)
  where
    (accept, q) = BS.breakSubstring ";q=" $ BS.filter (/= space) bs
    safeRead = fmap fst . listToMaybe . filter (null . snd) . reads


------------------------------------------------------------------------------
-- | Matches two lists of matchable values and picks the best matching
-- value. A result of 'Nothing' means that nothing matched (which should
-- indicate a 406 error). If two or more results arise with the same level
-- quality level and specificity, then the first one in the server list is
-- chosen.
--
-- The types are a little strange, but they allow the application of
-- 'MediaType', 'ByteString', and a choice of whether or not to mark the
-- values with quality values. The standard application of this function
-- will be in conjunction with 'parseAccepts', which means that the type
-- variable will correspond to Quality MediaType.
--
-- > parseAccepts header >>= match resourceTypeOptions
--
-- Use 'matchSimple' to drop the quality values from the resource type
-- options.
--
-- When matching media types, the order of the arguments is important for
-- correctly matching to the client-side preferences, because the client
-- can be less specific than the server but not vice-versa. For instance,
-- arguments of the following should not match, because the client is
-- specifically requesting `text/plain`, which the server *does not*
-- provide.
--
-- > match ["text" // "*" /! 1] ["text" // "plain" /! 1]
--
-- This does not apply in the other direction. In the following case the
-- result will be `text/plain`, because the client has requested any text
-- type and the server offers a more specific version of this.
--
-- > match ["text" // "plain" /! 1] ["text" // "*" /! 1]
--
-- The server need not be more specific, though: in the following case the
-- result is `text/*`.
--
-- > match ["text" // "*" /! 1] ["text" // "*" /! 1]
--
-- As per the spec, a more specific value will always be chosen, though.
-- This final example yields `text/plain`.
--
-- > match ["text" // "*" /! 1, "text" // "plain" /! 1] ["text" // "*" /! 1]
match :: Match a
      => [a]      -- ^ The server-side preferences
      -> [a]      -- ^ The client-side preferences
      -> Maybe a
match qm = specific . concatMap filterAndMap
  where
    filterAndMap u = map (mostSpecific u) $ filter (Match.matches u) qm
    specific (a : ms) = Just $ foldr mostSpecific a ms
    specific []       = Nothing


------------------------------------------------------------------------------
-- | As for 'match', but attaches a quality value of 1 to every server-side
-- preference.
--
-- If your resource doesn't prefer one media type to another, but you're
-- still matching against the result from 'parseAccepts', then this
-- function just saves you from writing `/! 1` after every type in your
-- options list.
--
-- > match ["text" // "plain" /! 1, "application" // "json" /! 1]
--
-- becomes
--
-- > matchQ1 ["text" // "plain", "application" // "json"]
matchQ1 :: Match a => [a] -> [Quality a] -> Maybe a
matchQ1 = (fmap unwrap .) . match . map (:! 1)


------------------------------------------------------------------------------
-- | The equivalent of `match` above, except the resulting choice is mapped
-- to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > maybe render406Error renderResource $ parseAccepts header >>= matchMap
-- >     [ ("text"        // "html" /! 1  , asHtml)
-- >     , ("application" // "json" /! 0.8, asJson)
-- >     ]
mapMatch :: Match a
         => [(a, b)]  -- ^ The map of server-side preferences to values
         -> [a]       -- ^ The client-side preferences
         -> Maybe b
mapMatch s c = match (map fst s) c >>= lookupMatches s
  where
    lookupMatches ((k, v) : r) a
        | Match.matches k a = Just v
        | otherwise         = lookupMatches r a
    lookupMatches [] _ = Nothing


------------------------------------------------------------------------------
-- | The obvious combination of 'matchMap' and 'matchQ1'. Avoids quality values
-- in the map's keys.
--
-- > maybe render406Error renderResource $ parseAccepts header >>= matchMapQ1
-- >     [ ("text"        // "html", asHtml)
-- >     , ("application" // "json", asJson)
-- >     ]
mapMatchQ1 :: Match a => [(a, b)] -> [Quality a] -> Maybe b
mapMatchQ1 = mapMatch . map f
  where
    f (a, b) = (a :! 1, b)

