{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
module Network.HTTP.Media.Tests (tests) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative               ((<$>), (<*>))
#endif
import Control.Monad                     ((>=>), replicateM)
import Data.ByteString                   (ByteString)
import Data.Foldable                     (foldlM)
import Data.Map                          (empty)
import Data.Maybe                        (isNothing, listToMaybe)
import Data.Monoid                       ((<>))
import Data.Word                         (Word16)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Media                    hiding (parameters, subType)
import Network.HTTP.Media.Gen                (padString)
import Network.HTTP.Media.MediaType.Gen
import Network.HTTP.Media.MediaType.Internal
import Network.HTTP.Media.Quality

------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testParse
    , testMatchAccept
    , testMapAccept
    , testMatchContent
    , testMapContent
    , testMatchQuality
    , testMapQuality
    ]


------------------------------------------------------------------------------
testParse :: Test
testParse = testGroup "parseQuality"
    [ testProperty "Without quality" $ do
        media    <- medias
        rendered <- padConcat (return . renderHeader) media
        return $ parseQuality rendered == Just (map maxQuality media)
    , testProperty "With quality" $ do
        media    <- qualities
        rendered <- padConcat padQuality media
        return $ parseQuality rendered == Just media
    , testProperty "With extensions" $ do
        media    <- qualities
        rendered <- padConcat (padQuality >=> padExtensions) media
        return $ parseQuality rendered == Just media
    ]
  where
    medias = listOf1 genMediaType
    qualities = medias >>= mapM (flip fmap (choose (0, 1000)) . Quality)
    padConcat f l = flip (foldlM (padComma f)) (tail l) =<< f (head l)
    padComma f a b = pad a <$> padString "," <*> f b
    padQuality qMedia = do
        semi <- padString ";"
        let d = renderHeader (qualityData qMedia)
            v = showQ (qualityValue qMedia)
        return $ d <> semi <> "q=" <> v
    padExtensions s = genParameters >>= fmap (s <>) . renderParameters
    pad a s b = a <> s <> b


------------------------------------------------------------------------------
testMatchAccept :: Test
testMatchAccept = testMatch "Accept" matchAccept renderHeader


------------------------------------------------------------------------------
testMapAccept :: Test
testMapAccept = testMap "Accept" mapAccept renderHeader


------------------------------------------------------------------------------
testMatchContent :: Test
testMatchContent = testGroup "matchContent"
    [ testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client = renderHeader
                [ MediaType "*" "*" empty
                , media { subType = "*" }
                , media { parameters = empty }
                , media
                ]
        return $ matchAccept [media] client == Just media
    , testProperty "Nothing" $ do
        (server, client) <- genServerAndClient
        let client' = filter (not . flip any server . matches) client
        return . isNothing $ matchAccept server (renderHeader client')
    , testProperty "Left biased" $ do
        server <- genServer
        return $
            matchAccept server (renderHeader server) == Just (head server)
    , testProperty "Against */*" $ do
        server <- genServer
        let stars = "*/*" :: ByteString
        return $
            matchAccept server (renderHeader [stars]) == Just (head server)
    , testProperty "Against type/*" $ do
        server <- genServer
        let client = renderHeader [subStarOf $ head server]
        return $ matchAccept server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMapContent :: Test
testMapContent = testGroup "mapContent"
    [ testProperty "Matches" $ do
        server <- genServer
        let zipped = zip server server
        return $ mapAccept zipped (renderHeader server) == listToMaybe server
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
        let zipped = zip server $ repeat ()
        return . isNothing $ mapAccept zipped (renderHeader client)
    ]


------------------------------------------------------------------------------
testMatchQuality :: Test
testMatchQuality = testMatch "Quality" matchQuality id


------------------------------------------------------------------------------
testMapQuality :: Test
testMapQuality = testMap "Quality" mapQuality id


------------------------------------------------------------------------------
testMatch
    :: String
    -> ([MediaType] -> a -> Maybe MediaType)
    -> ([Quality MediaType] -> a)
    -> Test
testMatch name match qToI = testGroup ("match" ++ name)
    [ testProperty "Highest quality" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 1000)
        let client = zipWith Quality server qs
            qmax v q = if qualityValue q > qualityValue v then q else v
        return $ match server (qToI client) ==
            Just (qualityData $ foldr1 qmax client)
    , testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client = qToI $ map maxQuality
                [ MediaType "*" "*" empty
                , media { subType = "*" }
                , media { parameters = empty }
                , media
                ]
        return $ match [media] client == Just media
    , testProperty "Nothing" $ do
        server <- genServer
        client <- listOf1 $ genDiffMediaTypesWith genConcreteMediaType server
        let client' = filter (not . flip any server . matches) client
        return . isNothing $ match server
            (qToI $ map maxQuality client')
    , testProperty "Never chooses q=0" $ do
        server <- genServer
        return . isNothing $
            match server (qToI $ map minQuality server)
    , testProperty "Left biased" $ do
        server <- genServer
        let client = qToI $ map maxQuality server
        return $ match server client == Just (head server)
    , testProperty "Against */*" $ do
        server <- genServer
        let stars = "*/*" :: MediaType
        return $ match server (qToI [maxQuality stars]) ==
            Just (head server)
    , testProperty "Against type/*" $ do
        server <- genServer
        let client = qToI [maxQuality (subStarOf $ head server)]
        return $ match server client == Just (head server)
    ]


------------------------------------------------------------------------------
testMap
    :: String
    -> ([(MediaType, MediaType)] -> a -> Maybe MediaType)
    -> ([Quality MediaType] -> a)
    -> Test
testMap name mapf qToI = testGroup ("map" ++ name)
    [ testProperty "Matches" $ do
        server <- genServer
        qs     <- replicateM (length server) $ choose (1, 1000 :: Word16)
        let client = zipWith Quality server qs
            qmax q v = if qualityValue q >= qualityValue v then q else v
            zipped = zip server server
        return $ mapf zipped (qToI client) ==
            Just (qualityData $ foldr1 qmax client)
    , testProperty "Nothing" $ do
        (server, client) <- genServerAndClient
        let zipped = zip server $ repeat "*/*"
        return . isNothing $ mapf zipped (qToI $ map maxQuality client)
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
