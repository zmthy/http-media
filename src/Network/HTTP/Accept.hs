{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | A framework for parsing Accept headers and choosing the correct media
-- type.
module Network.HTTP.Accept
    (
      -- * Media types
      MediaType
    , (//)
    , (/:)
    , (/?)
    , (/.)

      -- * Quality values
    , Quality
    , (/!)

      -- * Parsing
    , parseAccept
    ) where

------------------------------------------------------------------------------
import           Control.Monad (guard)
import           Data.ByteString (ByteString, split)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)
import           Data.Char (ord)
import           Data.Word (Word8)
------------------------------------------------------------------------------
import           Network.HTTP.Accept.MediaType
import           Network.HTTP.Accept.Quality
import           Network.HTTP.Accept.Utils


------------------------------------------------------------------------------
-- | Parses an Accept media type into a 'MediaType' with an attached
-- 'Quality'.
parseAccept :: ByteString -> Maybe (Quality MediaType)
parseAccept bs = do
    guard $ BS.elem slash m
    return . uncurry (/!) $ foldr f (a // b, 1) ps
  where
    (m : ps)   = split semi bs
    (a, b)     = breakByte slash m
    f s (t, q) = let p@(k, v) = breakByte equal s in
        if trimBS k == "q" then (t, read $ toString v) else (t /: p, q)

