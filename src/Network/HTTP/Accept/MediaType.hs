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

      -- * Querying
    , (/?)
    , (/.)

      -- * Utility functions
    , matches
    , isMoreSpecific
    ) where

------------------------------------------------------------------------------
import           Control.Monad (guard)
import           Data.ByteString (ByteString, split)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)
import           Data.Char (ord)
import           Data.Map (Map, empty, foldrWithKey, insert, isSubmapOf)
import qualified Data.Map as Map
import           Data.Word (Word8)
------------------------------------------------------------------------------
import           Network.HTTP.Accept.Utils


------------------------------------------------------------------------------
-- | An HTTP media type, consisting of the type, subtype, and parameters.
data MediaType
    = MediaType ByteString ByteString (Map ByteString ByteString)
    deriving (Eq)

instance Show MediaType where
    show (MediaType a b p) =
        foldrWithKey f (toString a ++ '/' : toString b) p
      where
        f k v = (++ ';' : toString k ++ '=' : toString v)


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
-- | Parses a MIME string into a 'MediaType'.
parse :: ByteString -> Maybe MediaType
parse bs = do
    guard $ BS.elem slash m
    return $ foldr (flip (/:) . breakByte equal) (a // b) ps
  where
    (m : ps) = split semi bs
    (a, b)   = breakByte slash m


------------------------------------------------------------------------------
-- | Evaluates whether either argument matches the other.
matches :: MediaType -> MediaType -> Bool
matches (MediaType a b p) (MediaType c d q) =
    (p == q &&) $ a == "*" || c == "*" ||
    (b == "*" || d == "*") && a == c || a == c && b == d


------------------------------------------------------------------------------
-- | Evaluates whether the left argument is more specific than the right.
isMoreSpecific :: MediaType -> MediaType -> Bool
isMoreSpecific (MediaType a b p) (MediaType c d q) =
    a /= "*" && (b /= "*" || c == "*") && (d == "*" || isSubmapOf q p)

