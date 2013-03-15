{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Network.HTTP.Accept.Tests (tests) where

------------------------------------------------------------------------------
import Data.ByteString.UTF8 (fromString)
import Data.List (intercalate)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Gen

------------------------------------------------------------------------------
import Network.HTTP.Accept
import Network.HTTP.Accept.MediaType.Gen


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testParse
    , testMatch
    , testMatchQ1
    , testMapMatch
    , testMapMatchQ1
    ]


------------------------------------------------------------------------------
testParse :: Test
testParse = testProperty "parseAccepts" $ do
    media <- listOf1 genMediaType
    let string = fromString $ intercalate "," (map show media)
    return $ parseAccepts string == Just (map (/! 1) media)


------------------------------------------------------------------------------
testMatch :: Test
testMatch = testProperty "match" True


------------------------------------------------------------------------------
testMatchQ1 :: Test
testMatchQ1 = testProperty "matchQ1" True


------------------------------------------------------------------------------
testMapMatch :: Test
testMapMatch = testProperty "mapMatch" True


------------------------------------------------------------------------------
testMapMatchQ1 :: Test
testMapMatchQ1 = testProperty "mapMatchQ1" True

