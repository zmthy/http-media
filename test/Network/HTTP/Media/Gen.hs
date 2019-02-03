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

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI

import           Control.Monad         (join, liftM2)
import           Data.ByteString       (ByteString)
import           Data.CaseInsensitive  (CI, original)
import           Data.Monoid           ((<>))
import           Test.QuickCheck.Gen   (Gen, elements, listOf, listOf1, scale)


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random characters from the given set.
genByteStringFrom :: String -> Gen ByteString
genByteStringFrom = fmap BS.pack . listOf1 . elements


------------------------------------------------------------------------------
genCIByteStringFrom :: String -> Gen (CI ByteString)
genCIByteStringFrom = fmap CI.mk . genByteStringFrom


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random alphanumeric characters with a
-- non-numeric head.
genByteString :: Gen ByteString
genByteString = fmap BS.pack . (:) <$>
    elements alpha <*> scale (max 0 . pred) (listOf (elements alphaNum))
  where
    alpha = ['a'..'z'] ++ ['A'..'Z']
    alphaNum = alpha ++ ['0'..'9']


------------------------------------------------------------------------------
-- | Produces a non-empty case-insensitive ByteString of random alphanumeric
-- characters with a non-numeric head.
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
-- | Produces a non-empty ByteString of random alphanumeric characters that
-- is case-insensitively different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString = fmap original . genDiffCIByteString . CI.mk


------------------------------------------------------------------------------
-- | Produces a non-empty case-insensitive ByteString of random alphanumeric
-- characters that is different to the given one.
genDiffCIByteString :: CI ByteString -> Gen (CI ByteString)
genDiffCIByteString = genDiffWith genCIByteString


------------------------------------------------------------------------------
-- | Pad a 'ByteString' with a random amount of tab and space characters.
padString :: ByteString -> Gen ByteString
padString c = join (liftM2 pad) (BS.pack <$> listOf (elements " \t"))
  where pad a b = a <> c <> b
