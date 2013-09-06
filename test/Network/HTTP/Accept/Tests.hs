{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Network.HTTP.Accept.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad (liftM, replicateM)
import Data.ByteString.UTF8 (fromString)
import Data.List (intercalate)
import Data.Map (empty)
import Data.Maybe (isNothing)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Accept hiding (parameters, subType)
import Network.HTTP.Accept.MediaType.Gen
import Network.HTTP.Accept.MediaType.Internal
import Network.HTTP.Accept.Quality


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testParse
    , testMatch
    , testMapMatch
    ]


------------------------------------------------------------------------------
testParse :: Test
testParse = testGroup "parseAccepts"
    [ testProperty "Without quality" $ do
        media <- medias
        return $
            parseAccepts (group media) == Just (map (flip Quality 1) media)
    , testProperty "With quality" $ do
        media <- medias >>= mapM (flip liftM genQ . Quality)
        return $ parseAccepts (group media) == Just media
    ]
  where
    medias = listOf1 genMediaType
    group media = fromString $ intercalate "," (map show media)
    genQ = liftM ((/ 1000) . fromIntegral :: Int -> Float) $ choose (0, 1)


------------------------------------------------------------------------------
testMatch :: Test
testMatch = testGroup "match"
    [ testProperty "Highest quality" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 10 :: Int)
        let client = zipWith Quality server $ map ((/ 10) . fromIntegral) qs
            qmax v q = if quality q > quality v then q else v
        {-return $ match server client == Just (unwrap $ foldr1 qmax client) ||-}
            {-traceShow (match server client) (traceShow client $ traceShow server False)-}
        return $ match server client == Just (unwrap $ foldr1 qmax client)
    , testProperty "Most specific" $ do
        media <- genMediaTypeWith noStar noStar
        let client = map (`Quality` 1)
                [ MediaType "*" "*" empty
                , media { subType = "*" }
                , media { parameters = empty }
                , media
                ]
        return $ match [media] client == Just media
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypes server
        return . isNothing $ match server (map (`Quality` 1) client)
    , testProperty "Never chooses q=0" $ do
        server <- genServer
        return . isNothing $ match server (map (`Quality` 0) server)
    , testProperty "Left biased" $ do
        server <- genServer
        let client = map (`Quality` 1) server
        return $ match server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMapMatch :: Test
testMapMatch = testGroup "mapMatch"
    [ testProperty "Matches" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (0, 10 :: Int)
        let client = zipWith Quality server $ map ((/ 10) . fromIntegral) qs
            qmax q v = if quality q > quality v then q else v
            zipped = zip server server
        return $ mapMatch zipped client == Just (unwrap $ foldr1 qmax client)
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypes server
        let zipped = zip server $ repeat ()
        return . isNothing $ mapMatch zipped (map (`Quality` 1) client)
    ]


------------------------------------------------------------------------------
genServer :: Gen [MediaType]
genServer = listOf1 $ genMediaTypeWith noStar noStar

