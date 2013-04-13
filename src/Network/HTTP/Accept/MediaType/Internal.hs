{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Defined to allow the constructor of 'MediaType' to be exposed to tests.
module Network.HTTP.Accept.MediaType.Internal
    ( MediaType (..)
    , Parameters
    ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.ByteString.UTF8 (toString)
import           Data.Map (Map, foldrWithKey)
import qualified Data.Map as Map

------------------------------------------------------------------------------
import           Network.HTTP.Accept.Match (Match (..))


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
    { -- | The main type of the MediaType.
      mainType   :: !ByteString
      -- | The sub type of the MediaType.
    , subType    :: !ByteString
      -- | The parameters of the MediaType.
    , parameters :: Parameters
    } deriving (Eq)

instance Show MediaType where
    show (MediaType a b p) =
        foldrWithKey f (toString a ++ '/' : toString b) p
      where
        f k v = (++ ';' : toString k ++ '=' : toString v)

instance Match MediaType where
    matches a b
        | mainType b == "*" = params
        | subType b == "*"  = mainType a == mainType b && params
        | otherwise         = main && sub && params
      where
        main = mainType a == mainType b
        sub = subType a == subType b
        params = Map.null (parameters b) || parameters a == parameters b

    moreSpecificThan a b
        | mainType a == "*" = anyB && params
        | subType a == "*"  = anyB || subB && params
        | otherwise         = anyB || subB || params
      where
        anyB = mainType b == "*"
        subB = subType b == "*"
        params = not (Map.null $ parameters a) && Map.null (parameters b)


------------------------------------------------------------------------------
-- | 'MediaType' parameters.
type Parameters = Map ByteString ByteString

