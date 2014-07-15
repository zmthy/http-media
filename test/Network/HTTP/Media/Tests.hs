------------------------------------------------------------------------------
module Network.HTTP.Media.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.ByteString                   (ByteString)
import Data.ByteString.UTF8              (fromString)
import Data.List                         (intercalate)
import Data.Map                          (empty)
import Data.Maybe                        (isNothing, listToMaybe)
import Data.Word                         (Word16)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Media                    hiding (parameters, subType)
import Network.HTTP.Media.MediaType.Gen
import Network.HTTP.Media.MediaType.Internal
import Network.HTTP.Media.Quality


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testMatchAccept
    , testMapAccept
    , testMatchContent
    , testMapContent
    ]


------------------------------------------------------------------------------
testMatchAccept :: Test
testMatchAccept = testGroup "matchAccept"
    [ testProperty "Highest quality" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 1000)
        let client = zipWith Quality server qs
            qmax v q = if qualityValue q > qualityValue v then q else v
        return $ matchAccept server (qToBS client) ==
            Just (qualityData $ foldr1 qmax client)
    , testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client = qToBS $ map maxQuality
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
        return . isNothing $ matchAccept server
            (qToBS $ map maxQuality client')
    , testProperty "Never chooses q=0" $ do
        server <- genServer
        return . isNothing $
            matchAccept server (qToBS $ map minQuality server)
    , testProperty "Left biased" $ do
        server <- genServer
        let client = qToBS $ map maxQuality server
        return $ matchAccept server client == Just (head server)
    , testProperty "Against */*" $ do
        server <- genServer
        let stars = "*/*" :: ByteString
        return $ matchAccept server (qToBS [maxQuality stars]) ==
            Just (head server)
    , testProperty "Against type/*" $ do
        server <- genServer
        let client = qToBS [maxQuality (subStarOf $ head server)]
        return $ matchAccept server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMapAccept :: Test
testMapAccept = testGroup "mapAccept"
    [ testProperty "Matches" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 1000 :: Word16)
        let client = zipWith Quality server qs
            qmax q v = if qualityValue q >= qualityValue v then q else v
            zipped = zip server server
        return $ mapAccept zipped (qToBS client) ==
            Just (qualityData $ foldr1 qmax client)
    , testProperty "Nothing" $ do
        (server, client) <- genServerAndClient
        let zipped = zip server $ repeat ()
        return . isNothing $ mapAccept zipped (qToBS $ map maxQuality client)
    ]


------------------------------------------------------------------------------
testMatchContent :: Test
testMatchContent = testGroup "matchContent"
    [ testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client = toBS
                [ MediaType "*" "*" empty
                , media { subType = "*" }
                , media { parameters = empty }
                , media
                ]
        return $ matchAccept [media] client == Just media
    , testProperty "Nothing" $ do
        (server, client) <- genServerAndClient
        let client' = filter (not . flip any server . matches) client
        return . isNothing $ matchAccept server (toBS client')
    , testProperty "Left biased" $ do
        server <- genServer
        return $ matchAccept server (toBS server) == Just (head server)
    , testProperty "Against */*" $ do
        server <- genServer
        let stars = "*/*" :: ByteString
        return $ matchAccept server (toBS [stars]) == Just (head server)
    , testProperty "Against type/*" $ do
        server <- genServer
        let client = toBS [subStarOf $ head server]
        return $ matchAccept server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMapContent :: Test
testMapContent = testGroup "mapContent"
    [ testProperty "Matches" $ do
        server <- genServer
        let zipped = zip server server
        return $ mapAccept zipped (toBS server) == listToMaybe server
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
        let zipped = zip server $ repeat ()
        return . isNothing $ mapAccept zipped (toBS client)
    ]


------------------------------------------------------------------------------
genServer :: Gen [MediaType]
genServer = listOf1 genConcreteMediaType


------------------------------------------------------------------------------
genServerAndClient :: Gen ([MediaType], [MediaType])
genServerAndClient = do
    server <- genServer
    client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
    return (server, client)


------------------------------------------------------------------------------
toBS :: Accept a => [a] -> ByteString
toBS = fromString . intercalate "," . map showAccept


------------------------------------------------------------------------------
qToBS :: Accept a => [Quality a] -> ByteString
qToBS = fromString . intercalate "," . map show

