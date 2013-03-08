{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Defines the media type types and functions.
module Network.HTTP.Accept.MediaType
    (
      -- * Type and creation
      MediaType
    , (//)
    , (/:)
    , parse
    , anything

      -- * Querying
    , mainType
    , subType
    , parameters
    , (/?)
    , (/.)
    , matches
    ) where

------------------------------------------------------------------------------
import           Control.Monad (guard)

import           Data.ByteString (ByteString, split)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)
import           Data.Map (Map, empty, foldrWithKey, insert, isSubmapOf)
import qualified Data.Map as Map

------------------------------------------------------------------------------
import           Network.HTTP.Accept.Match hiding (matches)
import qualified Network.HTTP.Accept.Match as Match
import           Network.HTTP.Accept.Utils


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType = MediaType
    { -- | The main type of the MediaType.
      mainType   :: ByteString
      -- | The sub type of the MediaType.
    , subType    :: ByteString
      -- | The parameters of the MediaType.
    , parameters :: Map ByteString ByteString
    } deriving (Eq)

instance Show MediaType where
    show (MediaType a b p) =
        foldrWithKey f (toString a ++ '/' : toString b) p
      where
        f k v = (++ ';' : toString k ++ '=' : toString v)

instance Match MediaType where
    matches (MediaType a b p) (MediaType c d q) = c == "*" ||
        d == "*" && a == c || q `isSubmapOf` p && a == c && b == d
    isMoreSpecific (MediaType a b p) (MediaType c d q) =
        a /= "*" && (b /= "*" || c == "*") && (d == "*" || isSubmapOf q p)
    combine = moreSpecific




------------------------------------------------------------------------------
-- | Builds a 'MediaType' without parameters.
(//) :: ByteString -> ByteString -> MediaType
a // b = MediaType (trimBS a) (trimBS b) empty


------------------------------------------------------------------------------
-- | Adds a parameter to a 'MediaType'.
(/:) :: MediaType -> (ByteString, ByteString) -> MediaType
(MediaType a b p) /: (k, v) = MediaType a b $ insert k v p


------------------------------------------------------------------------------
-- | Evaluates if a 'MediaType' has a parameter of the given name.
(/?) :: MediaType -> ByteString -> Bool
(MediaType _ _ p) /? k = Map.member k p


------------------------------------------------------------------------------
-- | Retrieves a parameter from a 'MediaType'.
(/.) :: MediaType -> ByteString -> Maybe ByteString
(MediaType _ _ p) /. k = Map.lookup k p


------------------------------------------------------------------------------
-- | A MediaType that matches anything.
anything :: MediaType
anything = "*" // "*"


------------------------------------------------------------------------------
-- | Parses a MIME string into a 'MediaType'.
parse :: ByteString -> Maybe MediaType
parse bs = do
    guard $ BS.elem slash m && (a /= "*" || b == "*")
    return $ foldr (flip (/:) . breakByte equal) (a // b) ps
  where
    (m : ps) = split semi bs
    (a, b)   = breakByte slash m


------------------------------------------------------------------------------
-- | Evaluates if the left argument matches the right one.
--
-- The order of the arguments is important: if the right argument is more
-- specific than the left, they will not be considered to match. The
-- following evalutes to 'False'.
--
-- > matches ("text" // "*") ("text" // "plain")
matches :: MediaType -> MediaType -> Bool
matches = Match.matches

