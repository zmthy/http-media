------------------------------------------------------------------------------
-- | Contains definitions for generating 'ByteString's.
module Network.HTTP.Accept.Gen
    ( genByteString
    , genDiffByteString
    ) where

------------------------------------------------------------------------------
import Control.Monad (liftM)

import Data.ByteString (ByteString, pack)

import Test.QuickCheck.Gen (Gen, choose, listOf1, oneof)


------------------------------------------------------------------------------
-- | Produces a ByteString of random alpha characters.
genByteString :: Gen ByteString
genByteString = liftM pack $
    listOf1 (oneof [choose (65, 90), choose (97, 122)])


------------------------------------------------------------------------------
-- | Produces a random ByteString different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString bs = do
    bs' <- genByteString
    if bs == bs' then genDiffByteString bs else return bs'


