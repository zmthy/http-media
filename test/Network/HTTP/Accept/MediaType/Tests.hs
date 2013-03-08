{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Network.HTTP.Accept.MediaType.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Monad (liftM2)

import qualified Data.Map as Map
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>))

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)

------------------------------------------------------------------------------
import           Network.HTTP.Accept.MediaType
import           Network.HTTP.Accept.MediaType.Gen


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testHas
    , testGet
    , testMatches
    , testParse
    ]


------------------------------------------------------------------------------
testHas :: Test
testHas = testGroup "(/?)"
    [ testProperty "True for property it has" $ do
        media <- genMediaType
        return $ all (media /?) (Map.keys $ parameters media)
    , testProperty "False for property it doesn't have" $ do
        media <- genMediaType
        return $ all (not . (media { parameters = Map.empty } /?))
            (Map.keys $ parameters media)
    ]


------------------------------------------------------------------------------
testGet :: Test
testGet = testGroup "(/.)"
    [ testProperty "Retrieves property it has" $ do
        media  <- genMediaType
        let is n v = (&& media /. n == Just v)
        return $ Map.foldrWithKey is True $ parameters media
    , testProperty "Nothing for property it doesn't have" $ do
        media <- genMediaType
        let is n _ = (&& isNothing (media { parameters = Map.empty } /. n))
        return $ Map.foldrWithKey is True $ parameters media
    ]


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Equal values match" $ do
        media <- genMediaType
        return $ matches media media
    , testProperty "Different types don't match" $ do
        media1 <- genMediaType
        media2 <- genDiffMediaType media1
        return . not $ matches media1 media2
    , testProperty "Same sub but different main don't match" $ do
        main1 <- genByteString
        main2 <- genDiffByteString main1
        sub   <- genByteString
        return . not $ matches (main1 // sub) (main2 // sub)
    , testProperty "Same main but different sub don't match" $ do
        main <- genByteString
        sub1 <- genByteString
        sub2 <- genDiffByteString sub1
        return . not $ matches (main // sub1) (main // sub2)
    , testGroup "Parameters"
        [ testProperty "Matches same parameters" $ do
            pm <- liftM2 addParameters genMediaType genParameters
            return $ matches pm pm
        , testProperty "Doesn't match different parameters" $ do
            media <- genMediaType
            params@((name, value1) : rest) <- genParameters
            value2 <- genDiffByteString value1
            let pm1 = addParameters media params
                pm2 = addParameters media ((name, value2) : rest)
            return . not $ matches pm1 pm2
        , testProperty "Matches with less parameters on the right" $ do
            (params1, params2) <- genMediaTypeAndLessParameters
            return $ matches params1 params2
        , testProperty "Doesn't match with less parameters on the left" $ do
            (params1, params2) <- genMediaTypeAndLessParameters
            return . not $ matches params2 params1
        ]
    , testGroup "*/*"
        [ testProperty "Matches itself" $ matches anything anything
        , testProperty "Matches anything on the right" $ do
            media <- genDiffMediaType anything
            return $ matches media anything
        , testProperty "Doesn't match more specific on the left" $ do
            media <- genDiffMediaType anything
            return . not $ matches anything media
        ]
    , testGroup "Sub *"
        [ testProperty "Matches itself" $ do
            main <- genByteString
            return $ matches (main // "*") (main // "*")
        , testProperty "Matches on the right" $ do
            main <- genByteString
            sub  <- genByteString
            return $ matches (main // sub) (main // "*")
        , testProperty "Doesn't match on the left" $ do
            main <- genByteString
            sub  <- genByteString
            return . not $ matches (main // "*") (main // sub)
        ]
    ]


------------------------------------------------------------------------------
testParse :: Test
testParse = testProperty "parse" $ do
    main <- genByteString
    sub  <- genByteString
    let (Just media) = parse $ main <> "/" <> sub
    return $ mainType media == main && subType media == sub


