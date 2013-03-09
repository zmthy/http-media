------------------------------------------------------------------------------
module Network.HTTP.Accept.Tests (tests) where

------------------------------------------------------------------------------
{-import Network.HTTP.Accept-}

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testParse
    , testMatch
    , testMatchQ1
    , testMapMatch
    , testMapMatchQ1
    ]

testParse :: Test
testParse = testProperty "parse" True

testMatch :: Test
testMatch = testProperty "match" True

testMatchQ1 :: Test
testMatchQ1 = testProperty "matchQ1" True

testMapMatch :: Test
testMapMatch = testProperty "mapMatch" True

testMapMatchQ1 :: Test
testMapMatchQ1 = testProperty "mapMatchQ1" True

