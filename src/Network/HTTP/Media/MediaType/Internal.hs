------------------------------------------------------------------------------
-- | Defined to allow the constructor of 'MediaType' to be exposed to tests.
module Network.HTTP.Media.MediaType.Internal
    ( MediaType (..)
    , Parameters
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map             as Map

------------------------------------------------------------------------------
import Control.Monad   (guard)
import Data.ByteString (ByteString)
import Data.String     (IsString (..))
import Data.Map        (Map)
import Data.Maybe      (fromMaybe)
import Data.Monoid     ((<>))

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept       (Accept (..))
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Media.Utils


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
    { mainType   :: ByteString  -- ^ The main type of the MediaType
    , subType    :: ByteString  -- ^ The sub type of the MediaType
    , parameters :: Parameters  -- ^ The parameters of the MediaType
    } deriving (Eq)

instance Show MediaType where
    show = BS.toString . renderHeader

instance IsString MediaType where
    fromString str = flip fromMaybe (parseAccept $ BS.fromString str) $
        error $ "Invalid media type literal " ++ str



instance Accept MediaType where
    parseAccept bs = do
        let pieces = map trimBS $ BS.split semi bs
        guard $ not (null pieces)
        let (m : ps) = pieces
            (a, b)   = breakByte slash m
        guard $ BS.elem slash m && (a /= "*" || b == "*")
        return $ MediaType a b $
            foldr (uncurry Map.insert . breakByte equal) Map.empty ps

    matches a b
        | mainType b == "*" = params
        | subType b == "*"  = mainType a == mainType b && params
        | otherwise         = main && sub && params
      where
        main = mainType a == mainType b
        sub = subType a == subType b
        params = Map.null (parameters b) || parameters a == parameters b

    moreSpecificThan a b = (a `matches` b &&) $
        mainType a == "*" && anyB && params ||
        subType a == "*" && (anyB || subB && params) ||
        anyB || subB || params
      where
        anyB = mainType b == "*"
        subB = subType b == "*"
        params = not (Map.null $ parameters a) && Map.null (parameters b)

instance RenderHeader MediaType where
    renderHeader (MediaType a b p) = Map.foldrWithKey f (a <> "/" <> b) p
      where
        f k v = (<> ";" <> k <> "=" <> v)


------------------------------------------------------------------------------
-- | 'MediaType' parameters.
type Parameters = Map ByteString ByteString
