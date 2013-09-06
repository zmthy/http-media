------------------------------------------------------------------------------
module Network.HTTP.Accept.Match.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad (liftM, liftM2)
import Distribution.TestSuite.QuickCheck
import Network.HTTP.Accept.Gen
import Network.HTTP.Accept.Match


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
testMoreSpecificThan :: Test
testMoreSpecificThan = testProperty "moreSpecificThan" $
    liftM2 ((not .) . moreSpecificThan) genByteString genByteString


------------------------------------------------------------------------------
testMostSpecific :: Test
testMostSpecific = testProperty "mostSpecific" $ do
    string <- genByteString
    liftM ((== string) . mostSpecific string) genByteString

