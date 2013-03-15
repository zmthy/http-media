{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Network.HTTP.Accept.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad (liftM)

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
testParse = testGroup "parseAccepts"
    [ testProperty "Without quality" $ do
        media <- medias
        return $ parseAccepts (group media) == Just (map (/! 1) media)
    , testProperty "With quality" $ do
        media <- medias >>= mapM (flip liftM genQ . (/!))
        return $ parseAccepts (group media) == Just media
    ]
  where
    medias = listOf1 genMediaType
    group media = fromString $ intercalate "," (map show media)
    genQ = liftM ((/ 1000) . fromIntegral :: Int -> Float) $ choose (0, 1)


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

