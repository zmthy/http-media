{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Network.HTTP.Accept.MediaType.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Monad (replicateM)

import           Data.ByteString.UTF8 (fromString)
import           Data.Map (empty, foldrWithKey, keys, toList)
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>), mconcat)

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

------------------------------------------------------------------------------
import           Network.HTTP.Accept.Match
    (matches, moreSpecificThan, mostSpecific)
import           Network.HTTP.Accept.MediaType
    hiding (mainType, subType, parameters)
{-import qualified Network.HTTP.Accept.MediaType as MediaType-}
import           Network.HTTP.Accept.MediaType.Internal
import           Network.HTTP.Accept.MediaType.Gen


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testEq
    , testShow
    , testHas
    , testGet
    , testMatches
    , testMoreSpecificThan
    , testMostSpecific
    , testParse
    ]


------------------------------------------------------------------------------
-- Equality is derived, but we test it here to get 100% coverage.
testEq :: Test
testEq = testGroup "Eq"
    [ testProperty "==" $ do
        media <- genMediaType
        return $ media == media
    , testProperty "/=" $ do
        media  <- genMediaType
        media' <- genDiffMediaType media
        return $ media /= media'
    ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testProperty "show" $ do
    media <- genMediaType
    return $ parse (fromString $ show media) == Just media


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
    , testProperty "Different parameters don't match" $ do
        media  <- genMediaTypeWith noStar noStar
        params <- diffParams $ parameters media
        return . not $ matches media media { parameters = params }
    , testGroup "*/*"
        [ testProperty "Matches itself" $ matches anything anything
        , testProperty "Matches anything on the right" $ do
            media <- genMediaType
            return $ matches media anything
        , testProperty "Doesn't match more specific on the left" $ do
            media <- genMediaTypeWith noStar mayStar
            return . not $ matches anything media
        ]
    , testGroup "type/*"
        [ testProperty "Matches itself" $ do
            media <- genMediaTypeWith noStar (return "*")
            return $ matches media media
        , testProperty "Matches on the right" $ do
            media <- genMediaTypeWith noStar noStar
            return $ matches media media { subType = "*" }
        , testProperty "Doesn't match on the left" $ do
            media <- genMediaTypeWith noStar noStar
            return . not $ matches media { subType = "*" } media
        ]
    ]


------------------------------------------------------------------------------
testMoreSpecificThan :: Test
testMoreSpecificThan = testGroup "isMoreSpecific"
    [ testProperty "Against */*" $ do
        media <- genMediaTypeWith noStar mayStar
        return $ media `moreSpecificThan` anything
    , testProperty "With */*" $ do
        media <- genDiffMediaType anything
        return . not $ anything `moreSpecificThan` media
    , testProperty "Against type/*" $ do
        media <- genMediaTypeWith noStar noStar
        return $ media `moreSpecificThan` media { subType = "*" }
    , testProperty "With type/*" $ do
        media <- genMediaTypeWith noStar noStar
        return . not $ media { subType = "*" } `moreSpecificThan` media
    , testProperty "With parameters" $ do
        media  <- genMediaType
        params <- someParams
        return $ moreSpecificThan
            media { parameters = params } media { parameters = empty }
    , testProperty "Different with parameters" $ do
        [media, media'] <- replicateM 2 $ genMediaTypeWith noStar noStar
        params <- someParams
        return $ moreSpecificThan
            media { parameters = params } media' { parameters = empty }
    , testProperty "Different parameters" $ do
        media   <- genMediaType
        params  <- someParams
        params' <- someParams
        return . not $ moreSpecificThan
            media { parameters = params } media { parameters = params' }
    ]


------------------------------------------------------------------------------
testMostSpecific :: Test
testMostSpecific = testGroup "mostSpecific"
    [ testProperty "With */*" $ do
        media <- genMediaTypeWith noStar mayStar
        return $ mostSpecific media anything == media &&
            mostSpecific anything media == media
    , testProperty "With type/*" $ do
        media  <- genMediaTypeWith noStar noStar
        media' <- genMediaTypeWith noStar $ return "*"
        return $ mostSpecific media media' == media &&
            mostSpecific media' media == media
    , testProperty "With parameters" $ do
        media  <- genMediaType
        params <- someParams
        let media'  = media { parameters = params }
            media'' = media { parameters = empty }
        return $ mostSpecific media' media'' == media' &&
            mostSpecific media'' media' == media'
    , testProperty "Left biased" $ do
        media  <- genMediaTypeWith noStar noStar
        media' <- genMediaTypeWith noStar noStar
        let media'' = media' { parameters = parameters media }
        return $ mostSpecific media media'' == media &&
            mostSpecific media'' media == media''
    ]


------------------------------------------------------------------------------
testParse :: Test
testParse = testProperty "parse" $ do
    media <- genMediaType
    let main   = mainType media
        sub    = subType media
        params = parameters media
    let (Just parsed) = parse $ main <> "/" <> sub <> mconcat
            (map (uncurry ((<>) . (<> "=") . (";" <>))) $ toList params)
    return $ parsed == media


------------------------------------------------------------------------------
anything :: MediaType
anything = MediaType "*" "*" empty

