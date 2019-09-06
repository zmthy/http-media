------------------------------------------------------------------------------
module Network.HTTP.Media.Accept.Tests (tests) where

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.QuickCheck     (testProperty)

import           Network.HTTP.Media.Accept
import           Network.HTTP.Media.Gen


------------------------------------------------------------------------------
tests :: [TestTree]
tests =
    [ testMatches
    , testMoreSpecificThan
    ]


------------------------------------------------------------------------------
testMatches :: TestTree
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
testMoreSpecificThan :: TestTree
testMoreSpecificThan = testProperty "moreSpecificThan" $
    (not .) . moreSpecificThan <$> genByteString <*> genByteString
