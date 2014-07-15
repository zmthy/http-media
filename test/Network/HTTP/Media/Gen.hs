------------------------------------------------------------------------------
-- | Contains definitions for generating 'ByteString's.
module Network.HTTP.Media.Gen
    ( genByteStringFrom
    , genByteString
    , genDiffByteStringWith
    , genDiffByteString
    ) where

------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Data.ByteString     (ByteString, pack)
import Data.Word           (Word8)
import Test.QuickCheck.Gen (Gen, elements, listOf1)


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random characters from the given set.
genByteStringFrom :: [Word8] -> Gen ByteString
genByteStringFrom validChars = pack <$> listOf1 (elements validChars)


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random alphanumeric characters.
genByteString :: Gen ByteString
genByteString = genByteStringFrom $ [48..57] ++ [65..90] ++ [97..122]


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString different to the given one using the
-- given generator.
genDiffByteStringWith :: Gen ByteString -> ByteString -> Gen ByteString
genDiffByteStringWith gen bs = do
    bs' <- gen
    if bs == bs' then genDiffByteStringWith gen bs else return bs'


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random alphanumeric characters
-- different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString = genDiffByteStringWith genByteString

