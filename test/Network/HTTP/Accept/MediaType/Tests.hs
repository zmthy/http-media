{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Network.HTTP.Accept.MediaType.Tests (tests) where

------------------------------------------------------------------------------
import Data.Map (empty, foldrWithKey, keys, toList)
import Data.Maybe (isNothing)
import Data.Monoid ((<>), mconcat)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

------------------------------------------------------------------------------
import Network.HTTP.Accept.MediaType
import Network.HTTP.Accept.MediaType.Gen


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
        return $ all (media /?) (keys $ parameters media)
    , testProperty "False for property it doesn't have" $ do
        media <- genMediaType
        return $ all (not . (media { parameters = empty } /?))
            (keys $ parameters media)
    ]


------------------------------------------------------------------------------
testGet :: Test
testGet = testGroup "(/.)"
    [ testProperty "Retrieves property it has" $ do
        media  <- genMediaType
        let is n v = (&& media /. n == Just v)
        return $ foldrWithKey is True $ parameters media
    , testProperty "Nothing for property it doesn't have" $ do
        media <- genMediaType
        let is n _ = (&& isNothing (media { parameters = empty } /. n))
        return $ foldrWithKey is True $ parameters media
    ]


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Equal values match" $ do
        media <- genMediaType
        return $ matches media media
    , testProperty "Same sub but different main don't match" $ do
        media <- genMediaTypeWith noStar mayStar
        main  <- genDiffByteString $ mainType media
        return $ not (matches media media { mainType = main }) &&
            not (matches media { mainType = main } media)
    , testProperty "Same main but different sub don't match" $ do
        media <- genMediaTypeWith noStar noStar
        sub   <- genDiffByteString $ subType media
        return . not $ matches media media { subType = sub } ||
            matches media { subType = sub } media
    , testGroup "Parameters"
        [ testProperty "Doesn't match different parameters" $ do
            media  <- genMediaTypeWith noStar noStar
            params <- diffParams $ parameters media
            return . not $ matches media media { parameters = params }
        , testProperty "Matches with less parameters on the right" $ do
            media  <- genMediaTypeWith noStar noStar
            params <- moreParams $ parameters media
            return $ matches media { parameters = params } media
        , testProperty "Doesn't match with less parameters on the left" $ do
            media  <- genMediaTypeWith noStar noStar
            params <- moreParams $ parameters media
            return . not $ matches media media { parameters = params }
        ]
    , testGroup "*/*"
        [ testProperty "Matches itself" $ matches anything anything
        , testProperty "Matches anything on the right" $ do
            media <- genMediaType
            return $ matches media anything
        , testProperty "Doesn't match more specific on the left" $ do
            media <- genMediaTypeWith noStar mayStar
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
    main   <- genByteString
    sub    <- genByteString
    params <- mayParams
    let (Just media) = parse $ main <> "/" <> sub <> mconcat
            (map (uncurry ((<>) . (<> "=") . (";" <>))) $ toList params)
    return $ mainType media == main && subType media == sub &&
        parameters media == params

