{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Defined to allow the constructor of 'MediaType' to be exposed to tests.
module Network.HTTP.Accept.MediaType.Internal
    ( MediaType (..)
    , Parameters
    , toByteString
    , parse
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map

------------------------------------------------------------------------------
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.String (IsString (..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

------------------------------------------------------------------------------
import Network.HTTP.Accept.Match (Match (..))
import Network.HTTP.Accept.Utils


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
    { mainType   :: ByteString  -- ^ The main type of the MediaType
    , subType    :: ByteString  -- ^ The sub type of the MediaType
    , parameters :: Parameters  -- ^ The parameters of the MediaType
    } deriving (Eq)

instance Show MediaType where
    show (MediaType a b p) =
        Map.foldrWithKey f (toString a ++ '/' : toString b) p
      where
        f k v = (++ ';' : toString k ++ '=' : toString v)

instance IsString MediaType where
    fromString str = flip fromMaybe (parse $ BS.fromString str) $
        error $ "Invalid media type literal " ++ str

instance Match MediaType where
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


------------------------------------------------------------------------------
-- | 'MediaType' parameters.
type Parameters = Map ByteString ByteString


------------------------------------------------------------------------------
-- | Parses a media type header into a 'MediaType'.
parse :: ByteString -> Maybe MediaType
parse bs = do
    let pieces = BS.split semi bs
    guard $ not (null pieces)
    let (m : ps) = pieces
        (a, b)   = breakByte slash m
    guard $ BS.elem slash m && (a /= "*" || b == "*")
    return $ MediaType a b $
        foldr (uncurry Map.insert . breakByte equal) Map.empty ps


------------------------------------------------------------------------------
-- | Converts 'MediaType' to 'ByteString'.
toByteString :: MediaType -> ByteString
toByteString (MediaType a b p) = Map.foldrWithKey f (a <> "/" <> b) p
  where
    f k v = (<> ";" <> k <> "=" <> v)

