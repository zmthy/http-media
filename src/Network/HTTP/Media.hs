-- | A framework for parsing HTTP media type headers.
module Network.HTTP.Media
  ( -- * Media types
    MediaType,
    (//),
    (/:),
    (/+),
    mainType,
    subType,
    structuredSyntaxSuffix,
    parameters,
    (/?),
    (/.),

    -- * Charsets
    Charset,

    -- * Encodings
    Encoding,

    -- * Languages
    Language,
    toParts,

    -- * Accept matching
    matchAccept,
    mapAccept,
    mapAcceptMedia,
    mapAcceptCharset,
    mapAcceptEncoding,
    mapAcceptLanguage,
    mapAcceptBytes,

    -- * Content matching
    matchContent,
    mapContent,
    mapContentMedia,
    mapContentCharset,
    mapContentEncoding,
    mapContentLanguage,

    -- * Quality values
    Quality (qualityData),
    quality,
    QualityOrder,
    qualityOrder,
    isAcceptable,
    maxQuality,
    minQuality,
    parseQuality,
    matchQuality,
    mapQuality,

    -- * Accept
    Accept (..),

    -- * Rendering
    RenderHeader (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard, (>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (find, foldl', maximumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Network.HTTP.Media.Accept as Accept
import Network.HTTP.Media.Charset as Charset
import Network.HTTP.Media.Encoding as Encoding
import Network.HTTP.Media.Language as Language
import Network.HTTP.Media.MediaType as MediaType
import Network.HTTP.Media.Quality
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Media.Utils (trimBS)

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
matchAccept ::
  (Accept a) =>
  -- | The server-side options
  [a] ->
  -- | The client-side header value
  ByteString ->
  Maybe a
matchAccept = (parseQuality >=>) . matchQuality

-- | The equivalent of 'matchAccept' above, except the resulting choice is
-- mapped to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > getHeader >>= maybe render406Error renderResource . mapAccept
-- >     [ ("text" // "html",        asHtml)
-- >     , ("application" // "json", asJson)
-- >     ]
mapAccept ::
  (Accept a) =>
  -- | The map of server-side preferences to values
  [(a, b)] ->
  -- | The client-side header value
  ByteString ->
  Maybe b
mapAccept = (parseQuality >=>) . mapQuality

-- | A specialisation of 'mapAccept' that only takes 'MediaType' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getHeader >>= maybe render406Error renderResource . mapAcceptMedia
-- >     [ ("text/html",        asHtml)
-- >     , ("application/json", asJson)
-- >     ]
mapAcceptMedia ::
  -- | The map of server-side preferences to values
  [(MediaType, b)] ->
  -- | The client-side header value
  ByteString ->
  Maybe b
mapAcceptMedia = mapAccept

-- | A specialisation of 'mapAccept' that only takes 'Charset' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getHeader >>= maybe render406Error renderResource . mapAcceptCharset
-- >     [ ("utf-8",    inUtf8)
-- >     , ("us-ascii", inAscii)
-- >     ]
mapAcceptCharset ::
  -- | The map of server-side preferences to values
  [(Charset, b)] ->
  -- | The client-side header value
  ByteString ->
  Maybe b
mapAcceptCharset = mapAccept

-- | A specialisation of 'mapAccept' that only takes 'Encoding' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getHeader >>= maybe render406Error renderResource . mapAcceptEncoding
-- >     [ ("compress", compress)
-- >     , ("identity", id)
-- >     ]
mapAcceptEncoding ::
  -- | The map of server-side preferences to values
  [(Encoding, b)] ->
  -- | The client-side header value
  ByteString ->
  Maybe b
mapAcceptEncoding = mapAccept

-- | A specialisation of 'mapAccept' that only takes 'Language' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getHeader >>= maybe render406Error renderResource . mapAcceptLanguage
-- >     [ ("en-gb", inBritishEnglish)
-- >     , ("fr",    inFrench)
-- >     ]
mapAcceptLanguage ::
  -- | The map of server-side preferences to values
  [(Language, b)] ->
  -- | The client-side header value
  ByteString ->
  Maybe b
mapAcceptLanguage = mapAccept

-- | A specialisation of 'mapAccept' that only takes 'ByteString' as its
-- input, to avoid ambiguous-type errors when using string literal
-- overloading.
--
-- > getHeader >>= maybe render406Error encodeResourceWith . mapAcceptBytes
-- >     [ ("abc", abc)
-- >     , ("xyz", xyz)
-- >     ]
mapAcceptBytes ::
  -- | The map of server-side preferences to values
  [(ByteString, b)] ->
  -- | The client-side header value
  ByteString ->
  Maybe b
mapAcceptBytes = mapAccept

-- | Matches a list of server-side parsing options against a the client-side
-- content value. A result of 'Nothing' means that nothing matched (which
-- should indicate a 415 error).
--
-- > matchContent ["application/json", "text/plain"] <$> getContentType
--
-- For more information on the matching process see RFC 2616, section 14.17.
matchContent ::
  (Accept a) =>
  -- | The server-side response options
  [a] ->
  -- | The client's request value
  ByteString ->
  Maybe a
matchContent = findMatch id

-- | The equivalent of 'matchContent' above, except the resulting choice is
-- mapped to another value.
--
-- > getContentType >>= maybe send415Error readRequestBodyWith . mapContent
-- >     [ ("application" // "json", parseJson)
-- >     , ("text" // "plain",       parseText)
-- >     ]
mapContent ::
  (Accept a) =>
  -- | The map of server-side responses
  [(a, b)] ->
  -- | The client request's header value
  ByteString ->
  Maybe b
mapContent options = fmap snd . findMatch fst options

-- | A specialisation of 'mapContent' that only takes 'MediaType' as its
-- input, to avoid ambiguous-type errors when using string literal
-- overloading.
--
-- > getContentType >>=
-- >     maybe send415Error readRequestBodyWith . mapContentMedia
-- >         [ ("application/json", parseJson)
-- >         , ("text/plain",       parseText)
-- >         ]
mapContentMedia ::
  -- | The map of server-side responses
  [(MediaType, b)] ->
  -- | The client request's header value
  ByteString ->
  Maybe b
mapContentMedia = mapContent

-- | A specialisation of 'mapContent' that only takes 'Charset' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getContentCharset >>=
-- >     maybe send415Error readRequestBodyWith . mapContentCharset
-- >         [ ("utf-8",    parseUtf8)
-- >         , ("us-ascii", parseAscii)
-- >         ]
mapContentCharset ::
  -- | The map of server-side responses
  [(Charset, b)] ->
  -- | The client request's header value
  ByteString ->
  Maybe b
mapContentCharset = mapContent

-- | A specialisation of 'mapContent' that only takes 'Encoding' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getContentEncoding >>=
-- >     maybe send415Error readRequestBodyWith . mapContentEncoding
-- >         [ ("compress", decompress)
-- >         , ("identity", id)
-- >         ]
mapContentEncoding ::
  -- | The map of server-side responses
  [(Encoding, b)] ->
  -- | The client request's header value
  ByteString ->
  Maybe b
mapContentEncoding = mapContent

-- | A specialisation of 'mapContent' that only takes 'Language' as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getContentLanguage >>=
-- >     maybe send415Error readRequestBodyWith . mapContentLanguage
-- >         [ ("en-gb", parseBritishEnglish)
-- >         , ("fr",    parseFrench)
-- >         ]
mapContentLanguage ::
  -- | The map of server-side responses
  [(Language, b)] ->
  -- | The client request's header value
  ByteString ->
  Maybe b
mapContentLanguage = mapContent

-- | Parses a full Accept header into a list of quality-valued media types.
parseQuality :: (Accept a) => ByteString -> Maybe [Quality a]
parseQuality = parseQuality' Proxy

parseQuality' :: (Accept a) => Proxy a -> ByteString -> Maybe [Quality a]
parseQuality' p = (. map trimBS . BS.split ',') . mapM $ \s ->
  let (accept, q) = fromMaybe (s, Nothing) $ if ext then findQ s else getQ s
   in maybe (pure maxQuality) (fmap (flip Quality) . readQ) q
        <*> parseAccept accept
  where
    ext = hasExtensionParameters p

    -- Split on ';', and check if a quality value is there. A value of Nothing
    -- indicates there was no parameter, whereas a value of Nothing in the
    -- pair indicates the parameter was not a quality value.
    getQ s =
      let (a, b) = trimBS <$> BS.breakEnd (== ';') s
       in if BS.null a
            then Nothing
            else
              Just
                ( BS.init a,
                  if BS.isPrefixOf "q=" b then Just (BS.drop 2 b) else Nothing
                )

    -- Trawl backwards through the string, ignoring extension parameters.
    findQ s = do
      let q = getQ s
      (a, m) <- q
      maybe (findQ a) (const q) m

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
matchQuality ::
  (Accept a) =>
  -- | The server-side options
  [a] ->
  -- | The pre-parsed client-side header value
  [Quality a] ->
  Maybe a
matchQuality = findQuality id

-- | The equivalent of 'matchQuality' above, except the resulting choice is
-- mapped to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > parseQuality header >>= maybe render406Error renderResource . mapQuality
-- >     [ ("text" // "html",        asHtml)
-- >     , ("application" // "json", asJson)
-- >     ]
mapQuality ::
  (Accept a) =>
  -- | The map of server-side preferences to values
  [(a, b)] ->
  -- | The client-side header value
  [Quality a] ->
  Maybe b
mapQuality options = fmap snd . findQuality fst options

-- | Find a match in a list of options against a ByteString using an 'Accept'
-- instance obtained by mapping the options to another type.
findMatch :: (Accept b) => (a -> b) -> [a] -> ByteString -> Maybe a
findMatch f options bs = do
  ctype <- parseAccept bs
  find (matches ctype . f) options

-- | Find a quality match between a list of options and a quality-marked list
-- of a different type, by mapping the type of the former to the latter.
findQuality :: (Accept a) => (b -> a) -> [b] -> [Quality a] -> Maybe b
findQuality f options acceptq = do
  guard $ not (null options)
  q <- maximumBy (compare `on` fmap qualityOrder) optionsq
  guard $ isAcceptable q
  return $ qualityData q
  where
    optionsq = reverse $ map addQuality options
    addQuality opt = withQValue opt <$> foldl' (mfold opt) Nothing acceptq
    withQValue opt q = q {qualityData = opt}
    mfold opt cur q
      | f opt `matches` qualityData q = mostSpecific q <$> cur <|> Just q
      | otherwise = cur
