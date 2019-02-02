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
import qualified Test.QuickCheck.Gen   as Gen

import           Control.Monad         (join, liftM2)
import           Data.ByteString       (ByteString)
import           Data.CaseInsensitive  (CI, original)
import           Data.Monoid           ((<>))
import           Test.QuickCheck.Gen   (Gen)


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random characters from the given set.
genByteStringFrom :: String -> Gen ByteString
genByteStringFrom = fmap BS.pack . Gen.listOf1 . Gen.elements


------------------------------------------------------------------------------
genCIByteStringFrom :: String -> Gen (CI ByteString)
genCIByteStringFrom = fmap CI.mk . genByteStringFrom


------------------------------------------------------------------------------
-- | Produces a non-empty ByteString of random alphanumeric characters with a
-- non-numeric head.
genByteString :: Gen ByteString
genByteString = fmap BS.pack . (:)
    <$> Gen.elements alpha
    <*> Gen.scale (max 0 . pred) (Gen.listOf (Gen.elements alphaNum))
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
genDiffWith gen = Gen.suchThat gen . (/=)


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
padString c = join (liftM2 pad) (BS.pack <$> Gen.listOf (Gen.elements " \t"))
  where pad a b = a <> c <> b
