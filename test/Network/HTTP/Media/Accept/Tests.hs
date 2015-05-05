------------------------------------------------------------------------------
module Network.HTTP.Media.Accept.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad                        (join, liftM, liftM2)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept
import Network.HTTP.Media.Gen


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testMatches
    , testMoreSpecificThan
    , testMostSpecific
    ]


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Does match" $ do
        string <- genByteString
        return $ matches string string
    , testProperty "Doesn't match" $ do
        string  <- genByteString
        string' <- genDiffByteString string
        return . not $ matches string string'
    ]


------------------------------------------------------------------------------
-- | Note that this test never actually generates any strings, as they are not
-- required for the 'moreSpecificThan' test.
testMoreSpecificThan :: Test
testMoreSpecificThan = testProperty "moreSpecificThan" $
    join (liftM2 ((not .) . moreSpecificThan)) genByteString


------------------------------------------------------------------------------
testMostSpecific :: Test
testMostSpecific = testProperty "mostSpecific" $ do
    string <- genByteString
    liftM ((== string) . mostSpecific string) genByteString
