{-# LANGUAGE NamedFieldPuns #-}

-- | Defined to allow the constructor of 'MediaType' to be exposed to tests.
module Network.HTTP.Media.MediaType.Internal
  ( MediaType (..),
    Parameters,
  )
where

import Control.Monad (foldM, guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI, original)
import qualified Data.CaseInsensitive as CI
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString (..))
import Network.HTTP.Media.Accept (Accept (..))
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Media.Utils (breakChar, trimBS)
import Prelude hiding ((<>))

-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
  { -- | The main type of the MediaType
    mainType :: CI ByteString,
    -- | The sub type of the MediaType
    subType :: CI ByteString,
    -- | Structured syntax suffix (e.g., @+json@), see RFC 6839
    structuredSyntaxSuffix :: Maybe ByteString,
    -- | The parameters of the MediaType
    parameters :: Parameters
  }
  deriving (Eq, Ord)

instance Show MediaType where
  show = BS.unpack . renderHeader

instance IsString MediaType where
  fromString str =
    flip fromMaybe (parseAccept $ BS.pack str) $
      error $
        "Invalid media type literal " ++ str

instance Accept MediaType where
  parseAccept bs = do
    (s, ps) <- uncons (map trimBS (BS.split ';' bs))
    (mainType, rest) <- breakChar '/' s
    let (subType, structuredSyntaxSuffix) = case breakChar '+' rest of
          Nothing -> (rest, Nothing)
          Just (sub, suf) -> (sub, Just suf)
    guard $ not (BS.null mainType || BS.null subType) && (mainType /= "*" || subType == "*")
    parameters <- foldM insert Map.empty ps
    pure $
      MediaType
        { mainType = CI.mk mainType,
          subType = CI.mk subType,
          structuredSyntaxSuffix,
          parameters
        }
    where
      both f (a, b) = (f a, f b)
      insert ps =
        fmap (flip (uncurry Map.insert) ps . both CI.mk) . breakChar '='

  matches a b
    | mainType b == "*" = suffix && params
    | subType b == "*" = main && suffix && params
    | otherwise = main && sub && suffix && params
    where
      main = mainType a == mainType b
      sub = subType a == subType b
      suffix = case (structuredSyntaxSuffix a, structuredSyntaxSuffix b) of
        (Nothing, Nothing) -> True
        (Just sa, Just sb) -> sa == sb
        -- Allow a suffix on the matchee only if our pattern matches any
        -- subtype. This ensures */* will still match everything.
        (Just _, Nothing) -> subType b == "*"
        -- If the pattern specifies a suffix, it must be present on
        -- the matchee.
        (Nothing, Just _) -> False
      params = Map.null (parameters b) || parameters a == parameters b

  moreSpecificThan a b =
    (a `matches` b &&) $
      mainType a == "*" && anyB && params
        || subType a == "*" && (anyB || subB && params)
        || anyB
        || subB
        || params
    where
      anyB = mainType b == "*"
      subB = subType b == "*"
      params = not (Map.null $ parameters a) && Map.null (parameters b)

  hasExtensionParameters _ = True

instance RenderHeader MediaType where
  renderHeader MediaType {mainType, subType, parameters, structuredSyntaxSuffix} =
    Map.foldrWithKey f type_ parameters
    where
      type_ =
        mconcat $
          [ original mainType,
            "/",
            original subType,
            case structuredSyntaxSuffix of
              Nothing -> mempty
              Just s -> "+" <> s
          ]
      f k v = (<> ";" <> original k <> "=" <> original v)

-- | 'MediaType' parameters.
type Parameters = Map (CI ByteString) (CI ByteString)
