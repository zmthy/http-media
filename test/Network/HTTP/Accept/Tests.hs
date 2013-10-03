------------------------------------------------------------------------------
module Network.HTTP.Accept.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.ByteString.UTF8              (fromString)
import Data.List                         (intercalate)
import Data.Map                          (empty)
import Data.Maybe                        (isNothing)
import Data.Word                         (Word16)
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
            parseAccept (group media) == Just (map maxQuality media)
    , testProperty "With quality" $ do
        media <- medias >>= mapM (flip fmap (choose (0, 1000)) . Quality)
        return $ parseAccept (group media) == Just media
    ]
  where
    medias = listOf1 genMediaType
    group media = fromString $ intercalate "," (map show media)


------------------------------------------------------------------------------
testMatch :: Test
testMatch = testGroup "match"
    [ testProperty "Highest quality" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 1000)
        let client = zipWith Quality server qs
            qmax v q = if qualityValue q > qualityValue v then q else v
        return $ matchAccept server client ==
            Just (qualityData $ foldr1 qmax client)
    , testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client = map maxQuality
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
        return . isNothing $ matchAccept server (map maxQuality client')
    , testProperty "Never chooses q=0" $ do
        server <- genServer
        return . isNothing $ matchAccept server (map minQuality server)
    , testProperty "Left biased" $ do
        server <- genServer
        let client = map maxQuality server
        return $ matchAccept server client == Just (head server)
    , testProperty "Against */*" $ do
        server <- genServer
        return $ matchAccept server [maxQuality "*/*"] == Just (head server)
    , testProperty "Against type/*" $ do
        server <- genServer
        let client = [maxQuality (subStarOf $ head server)]
        return $ matchAccept server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMap :: Test
testMap = testGroup "map"
    [ testProperty "Matches" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 1000 :: Word16)
        let client = zipWith Quality server qs
            qmax q v = if qualityValue q >= qualityValue v then q else v
            zipped = zip server server
        return $ mapAccept zipped client ==
            Just (qualityData $ foldr1 qmax client)
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
        let zipped = zip server $ repeat ()
        return . isNothing $ mapAccept zipped (map maxQuality client)
    ]


------------------------------------------------------------------------------
genServer :: Gen [MediaType]
genServer = listOf1 genConcreteMediaType

