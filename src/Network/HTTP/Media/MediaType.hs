-- | Defines the 'MediaType' accept header with an 'Accept' instance for use
-- in content-type negotiation.
module Network.HTTP.Media.MediaType
  ( -- * Type and creation
    MediaType,
    Parameters,
    (//),
    (/:),
    (/+),

    -- * Querying
    mainType,
    subType,
    structuredSyntaxSuffix,
    parameters,
    (/?),
    (/.),
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (empty, insert)
import qualified Data.Map as Map
import Network.HTTP.Media.MediaType.Internal (MediaType (MediaType))
import Network.HTTP.Media.MediaType.Internal hiding (MediaType (..))
import qualified Network.HTTP.Media.MediaType.Internal as Internal
import Network.HTTP.Media.Utils

-- | Retrieves the main type of a 'MediaType'.
mainType :: MediaType -> CI ByteString
mainType = Internal.mainType

-- | Retrieves the sub type of a 'MediaType'.
subType :: MediaType -> CI ByteString
subType = Internal.subType

-- | Retrieves the structured syntax suffix (if any) of a 'MediaType'.
--
-- /See:/ [RFC 6839](https://www.rfc-editor.org/rfc/rfc6839)
structuredSyntaxSuffix :: MediaType -> Maybe ByteString
structuredSyntaxSuffix = Internal.structuredSyntaxSuffix

-- | Retrieves the parameters of a 'MediaType'.
parameters :: MediaType -> Parameters
parameters = Internal.parameters

-- | Builds a 'MediaType' without parameters. Can produce an error if
-- either type is invalid.
(//) :: ByteString -> ByteString -> MediaType
a // b
  | a == "*" && b == "*" = MediaType (CI.mk a) (CI.mk b) Nothing empty
  | b == "*" = MediaType (ensureR a) (CI.mk b) Nothing empty
  | otherwise = MediaType (ensureR a) (ensureR b) Nothing empty

-- | Adds a parameter to a 'MediaType'. Can produce an error if either
-- string is invalid.
(/:) :: MediaType -> (ByteString, ByteString) -> MediaType
m@MediaType {Internal.parameters = ps} /: (k, v) =
  m {Internal.parameters = insert (ensureR k) (ensureV v) ps}

-- | Adds/replaces a structured syntax suffix (like @+json@) on a 'MediaType'.
--
-- /See:/ [RFC 6839](https://www.rfc-editor.org/rfc/rfc6839)
(/+) :: MediaType -> ByteString -> MediaType
m /+ s = m {Internal.structuredSyntaxSuffix = Just s}

-- | Evaluates if a 'MediaType' has a parameter of the given name.
(/?) :: MediaType -> ByteString -> Bool
m /? k = Map.member (CI.mk k) $ parameters m

-- | Retrieves a parameter from a 'MediaType'.
(/.) :: MediaType -> ByteString -> Maybe (CI ByteString)
m /. k = Map.lookup (CI.mk k) $ parameters m

-- | Ensures that the 'ByteString' matches the ABNF for `reg-name` in RFC
-- 4288.
ensureR :: ByteString -> CI ByteString
ensureR bs =
  CI.mk $
    if l == 0 || l > 127
      then error $ "Invalid length for " ++ show bs
      else ensure isMediaChar bs
  where
    l = BS.length bs

-- | Ensures that the 'ByteString' does not contain invalid characters for
-- a parameter value. RFC 4288 does not specify what characters are valid, so
-- here we just disallow parameter and media type breakers, ',' and ';'.
ensureV :: ByteString -> CI ByteString
ensureV = CI.mk . ensure (`notElem` [',', ';'])

-- | Ensures the predicate matches for every character in the given string.
ensure :: (Char -> Bool) -> ByteString -> ByteString
ensure f bs =
  maybe
    (error $ "Invalid character in " ++ show bs)
    (const bs)
    (BS.find f bs)
