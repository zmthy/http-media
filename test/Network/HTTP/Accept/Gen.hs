------------------------------------------------------------------------------
-- | Contains definitions for generating 'ByteString's.
module Network.HTTP.Accept.Gen
    ( genByteString
    , genDiffByteString
    ) where

------------------------------------------------------------------------------
import Control.Monad (liftM)

import Data.ByteString (ByteString, pack)

import Test.QuickCheck.Gen (Gen, listOf1, oneof)

------------------------------------------------------------------------------
{-import Network.HTTP.Accept.Utils-}


------------------------------------------------------------------------------
-- | Produces a ByteString of random alpha characters.
genByteString :: Gen ByteString
genByteString = liftM pack $ listOf1 (oneof $ map return validChars)
  where
    validChars = [48..57] ++ [65..90] ++ [97..122]


------------------------------------------------------------------------------
-- | Produces a random ByteString different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString bs = do
    bs' <- genByteString
    if bs == bs' then genDiffByteString bs else return bs'


