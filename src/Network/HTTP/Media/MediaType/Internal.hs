------------------------------------------------------------------------------
-- | Defined to allow the constructor of 'MediaType' to be exposed to tests.
module Network.HTTP.Media.MediaType.Internal
    ( MediaType (..)
    , Parameters
    ) where

import qualified Data.ByteString.Char8           as BS
import qualified Data.CaseInsensitive            as CI
import qualified Data.Map                        as Map

import           Control.Monad                   (foldM, guard)
import           Data.ByteString                 (ByteString)
import           Data.CaseInsensitive            (CI, original)
import           Data.Map                        (Map)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.String                     (IsString (..))

import           Network.HTTP.Media.Accept       (Accept (..))
import           Network.HTTP.Media.RenderHeader (RenderHeader (..))
import           Network.HTTP.Media.Utils        (breakChar, trimBS)


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
    { mainType   :: CI ByteString  -- ^ The main type of the MediaType
    , subType    :: CI ByteString  -- ^ The sub type of the MediaType
    , parameters :: Parameters     -- ^ The parameters of the MediaType
    } deriving (Eq, Ord)

instance Show MediaType where
    show = BS.unpack . renderHeader

instance IsString MediaType where
    fromString str = flip fromMaybe (parseAccept $ BS.pack str) $
        error $ "Invalid media type literal " ++ str

instance Accept MediaType where
    parseAccept bs = do
        (s, ps) <- uncons (map trimBS (BS.split ';' bs))
        (a, b)  <- breakChar '/' s
        guard $ not (BS.null a || BS.null b) && (a /= "*" || b == "*")
        ps' <- foldM insert Map.empty ps
        return $ MediaType (CI.mk a) (CI.mk b) ps'
      where
        uncons []      = Nothing
        uncons (a : b) = Just (a, b)
        both f (a, b) = (f a, f b)
        insert ps =
          fmap (flip (uncurry Map.insert) ps . both CI.mk) . breakChar '='

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

    hasExtensionParameters _ = True

instance RenderHeader MediaType where
    renderHeader (MediaType a b p) =
        Map.foldrWithKey f (original a <> "/" <> original b) p
      where
        f k v = (<> ";" <> original k <> "=" <> original v)


------------------------------------------------------------------------------
-- | 'MediaType' parameters.
type Parameters = Map (CI ByteString) (CI ByteString)
