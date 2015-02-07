------------------------------------------------------------------------------
-- | Defined to allow the constructor of 'MediaType' to be exposed to tests.
module Network.HTTP.Media.MediaType.Internal
    ( MediaType (..)
    , Parameters
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI
import qualified Data.Map              as Map

------------------------------------------------------------------------------
import Control.Monad        (guard)
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI, original)
import Data.String          (IsString (..))
import Data.Map             (Map)
import Data.Maybe           (fromMaybe)
import Data.Monoid          ((<>))

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept       (Accept (..))
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Media.Utils        (breakChar)


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
    { mainType   :: CI ByteString  -- ^ The main type of the MediaType
    , subType    :: CI ByteString  -- ^ The sub type of the MediaType
    , parameters :: Parameters     -- ^ The parameters of the MediaType
    } deriving (Eq)

instance Show MediaType where
    show = BS.unpack . renderHeader

instance IsString MediaType where
    fromString str = flip fromMaybe (parseAccept $ BS.pack str) $
        error $ "Invalid media type literal " ++ str

instance Accept MediaType where
    parseAccept bs = do
        let pieces = BS.split ';' bs
        guard $ not (null pieces)
        let (m : ps) = pieces
            (a, b)   = both CI.mk (breakChar '/' m)
        guard $ BS.elem '/' m && (a /= "*" || b == "*")
        return $ MediaType a b (foldr insert Map.empty ps)
      where
        both f (a, b) = (f a, f b)
        insert = uncurry Map.insert . both CI.mk . breakChar '='

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
    renderHeader (MediaType a b p) =
        Map.foldrWithKey f (original a <> "/" <> original b) p
      where
        f k v = (<> ";" <> original k <> "=" <> original v)


------------------------------------------------------------------------------
-- | 'MediaType' parameters.
type Parameters = Map (CI ByteString) (CI ByteString)
