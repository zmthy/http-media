------------------------------------------------------------------------------
-- | Defines the 'MediaType' accept header with an 'Accept' instance for use
-- in content-type negotiation.
module Network.HTTP.Media.MediaType
    (
    -- * Type and creation
      MediaType
    , Parameters
    , (//)
    , (/:)

    -- * Querying
    , mainType
    , subType
    , parameters
    , (/?)
    , (/.)
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI
import qualified Data.Map              as Map

------------------------------------------------------------------------------
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI)
import Data.Map             (empty, insert)

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.MediaType.Internal as Internal

------------------------------------------------------------------------------
import Network.HTTP.Media.MediaType.Internal (MediaType (MediaType))
import Network.HTTP.Media.MediaType.Internal hiding (MediaType (..))
import Network.HTTP.Media.Utils


------------------------------------------------------------------------------
-- | Retrieves the main type of a 'MediaType'.
mainType :: MediaType -> CI ByteString
mainType = Internal.mainType


------------------------------------------------------------------------------
-- | Retrieves the sub type of a 'MediaType'.
subType :: MediaType -> CI ByteString
subType = Internal.subType


------------------------------------------------------------------------------
-- | Retrieves the parameters of a 'MediaType'.
parameters :: MediaType -> Parameters
parameters = Internal.parameters


------------------------------------------------------------------------------
-- | Builds a 'MediaType' without parameters. Can produce an error if
-- either type is invalid.
(//) :: ByteString -> ByteString -> MediaType
a // b = MediaType (ensureR a) (ensureR b) empty


------------------------------------------------------------------------------
-- | Adds a parameter to a 'MediaType'. Can produce an error if either
-- string is invalid.
(/:) :: MediaType -> (ByteString, ByteString) -> MediaType
(MediaType a b p) /: (k, v) = MediaType a b $ insert (ensureR k) (ensureV v) p


------------------------------------------------------------------------------
-- | Evaluates if a 'MediaType' has a parameter of the given name.
(/?) :: MediaType -> ByteString -> Bool
(MediaType _ _ p) /? k = Map.member (CI.mk k) p


------------------------------------------------------------------------------
-- | Retrieves a parameter from a 'MediaType'.
(/.) :: MediaType -> ByteString -> Maybe (CI ByteString)
(MediaType _ _ p) /. k = Map.lookup (CI.mk k) p


------------------------------------------------------------------------------
-- | Ensures that the 'ByteString' matches the ABNF for `reg-name` in RFC
-- 4288.
ensureR :: ByteString -> CI ByteString
ensureR bs = CI.mk $ if l == 0 || l > 127
    then error $ "Invalid length for " ++ show bs else ensure isValidChar bs
  where l = BS.length bs


------------------------------------------------------------------------------
-- | Ensures that the 'ByteString' does not contain invalid characters for
-- a parameter value. RFC 4288 does not specify what characters are valid, so
-- here we just disallow parameter and media type breakers, ',' and ';'.
ensureV :: ByteString -> CI ByteString
ensureV = CI.mk . ensure (`notElem` [',', ';'])


------------------------------------------------------------------------------
-- | Ensures the predicate matches for every character in the given string.
ensure :: (Char -> Bool) -> ByteString -> ByteString
ensure f bs = maybe
    (error $ "Invalid character in " ++ show bs) (const bs) (BS.find f bs)
