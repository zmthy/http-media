------------------------------------------------------------------------------
-- | Contains definitions for generating 'ByteString's.
module Network.HTTP.Media.Gen
    ( genByteStringFrom
    , genCIByteStringFrom
    , genByteString
    , genCIByteString
    , genDiffWith
    , genDiffByteString
    , genDiffCIByteString

    , padString
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI

------------------------------------------------------------------------------
import Control.Applicative  ((<$>))
import Control.Monad        (liftM2, join)
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI)
import Data.Monoid          ((<>))
import Test.QuickCheck.Gen  (Gen, elements, listOf, listOf1)


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random characters from the given set.
genByteStringFrom :: [Char] -> Gen ByteString
genByteStringFrom = fmap BS.pack . listOf1 . elements


------------------------------------------------------------------------------
genCIByteStringFrom :: [Char] -> Gen (CI ByteString)
genCIByteStringFrom = fmap CI.mk . genByteStringFrom


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random alphanumeric characters.
genByteString :: Gen ByteString
genByteString = genByteStringFrom (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])


------------------------------------------------------------------------------
genCIByteString :: Gen (CI ByteString)
genCIByteString = fmap CI.mk genByteString


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString different to the given one using the
-- given generator.
genDiffWith :: Eq a => Gen a -> a -> Gen a
genDiffWith gen a = do
    b <- gen
    if a == b then genDiffWith gen a else return b


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random alphanumeric characters
-- different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString = genDiffWith genByteString


------------------------------------------------------------------------------
genDiffCIByteString :: CI ByteString -> Gen (CI ByteString)
genDiffCIByteString = genDiffWith genCIByteString


------------------------------------------------------------------------------
-- | Pad a 'ByteString' with a random amount of tab and space characters.
padString :: ByteString -> Gen (CI ByteString)
padString c =
    CI.mk <$> join (liftM2 pad) (BS.pack <$> listOf (elements " \t"))
  where pad a b = a <> c <> b
