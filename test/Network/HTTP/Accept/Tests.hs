------------------------------------------------------------------------------
module Network.HTTP.Accept.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad                     (liftM, replicateM)
import Data.ByteString.UTF8              (fromString)
import Data.List                         (intercalate)
import Data.Map                          (empty)
import Data.Maybe                        (isNothing)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Accept                    hiding (parameters, subType)
import Network.HTTP.Accept.MediaType.Gen
import Network.HTTP.Accept.MediaType.Internal
import Network.HTTP.Accept.Quality


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testParse
    , testMatch
    , testMap
    ]


------------------------------------------------------------------------------
testParse :: Test
testParse = testGroup "parseAccept"
    [ testProperty "Without quality" $ do
        media <- medias
        return $
            parseAccept (group media) == Just (map (flip Quality 1) media)
    , testProperty "With quality" $ do
        media <- medias >>= mapM (flip liftM genQ . Quality)
        return $ parseAccept (group media) == Just media
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
        return $ matchAccept server client == Just (unwrap $ foldr1 qmax client)
    , testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client = map (`Quality` 1)
                [ MediaType "*" "*" empty
                , media { subType = "*" }
                , media { parameters = empty }
                , media
                ]
        return $ matchAccept [media] client == Just media
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
        let client' = filter (not . flip any server . matches) client
        return . isNothing $ matchAccept server (map (`Quality` 1) client')
    , testProperty "Never chooses q=0" $ do
        server <- genServer
        return . isNothing $ matchAccept server (map (`Quality` 0) server)
    , testProperty "Left biased" $ do
        server <- genServer
        let client = map (`Quality` 1) server
        return $ matchAccept server client == Just (head server)
    , testProperty "Against */*" $ do
        server <- genServer
        return $ matchAccept server [Quality "*/*" 1] == Just (head server)
    , testProperty "Against type/*" $ do
        server <- genServer
        let client = [Quality (subStarOf $ head server) 1]
        return $ matchAccept server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMap :: Test
testMap = testGroup "map"
    [ testProperty "Matches" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 10 :: Int)
        let client = zipWith Quality server $ map ((/ 10) . fromIntegral) qs
            qmax q v = if quality q >= quality v then q else v
            zipped = zip server server
        return $ mapAccept zipped client == Just (unwrap $ foldr1 qmax client)
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
        let zipped = zip server $ repeat ()
        return . isNothing $ mapAccept zipped (map (`Quality` 1) client)
    ]


------------------------------------------------------------------------------
genServer :: Gen [MediaType]
genServer = listOf1 genConcreteMediaType

