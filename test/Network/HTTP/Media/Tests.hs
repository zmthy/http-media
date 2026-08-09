{-# LANGUAGE TupleSections #-}

module Network.HTTP.Media.Tests (tests) where

import Control.Monad (join, replicateM, (>=>))
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.List (nubBy)
import Data.Map (empty)
import Data.Monoid ((<>))
import Data.Word (Word16)
import Network.HTTP.Media hiding
  ( parameters,
    subType,
  )
import Network.HTTP.Media.Gen (padString)
import Network.HTTP.Media.MediaType.Gen
import Network.HTTP.Media.MediaType.Internal
import Network.HTTP.Media.Quality
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding ((<>))

tests :: [TestTree]
tests =
  [ testParse,
    testMatchAccept,
    testMapAccept,
    testMatchContent,
    testMapContent,
    testMatchQuality,
    testMapQuality
  ]

testParse :: TestTree
testParse =
  testGroup
    "parseQuality"
    [ testProperty "Without quality" $ do
        media <- medias
        rendered <- padConcat (return . renderHeader) media
        return $ parseQuality rendered === Just (map maxQuality media),
      testProperty "With quality" $ do
        media <- qualities
        rendered <- padConcat padQuality media
        return $ parseQuality rendered === Just media,
      testProperty "With extensions" $ do
        media <- qualities
        rendered <- padConcat (padQuality >=> padExtensions) media
        return $ parseQuality rendered === Just media
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

testMatchAccept :: TestTree
testMatchAccept = testMatch "Accept" matchAccept renderHeader

testMapAccept :: TestTree
testMapAccept = testMap "Accept" mapAccept renderHeader

testMatchContent :: TestTree
testMatchContent =
  testGroup
    "matchContent"
    [ testProperty "Matches" $ do
        media <- genMediaType
        return $ matchContent [media] (renderHeader media) === Just media,
      testProperty "Nothing" $ do
        content <- genMediaType
        parsers <- filter (not . matches content) <$> genServer
        return $ matchContent parsers (renderHeader content) === Nothing,
      testProperty "Against */*" $ do
        media <- genMediaType
        return $
          matchContent [anything] (renderHeader media) === Just anything,
      testProperty "Against type/*" $ do
        media <- genMediaType
        let sub = subStarOf media
        return $ matchContent [sub] (renderHeader media) === Just sub
    ]

testMapContent :: TestTree
testMapContent =
  testGroup
    "mapContent"
    [ testProperty "Matches" $ do
        media <- genMediaType
        return $ mapContent [(media, ())] (renderHeader media) === Just (),
      testProperty "Nothing" $ do
        content <- genMediaType
        parsers <- join zip . filter (not . matches content) <$> genServer
        return $ mapContent parsers (renderHeader content) === Nothing,
      testProperty "Overlapping keys" $ do
        (a, b) <- genMatchingPair
        return $ mapContent [(a, False), (b, True)] (renderHeader b)
    ]

testMatchQuality :: TestTree
testMatchQuality = testMatch "Quality" matchQuality id

testMapQuality :: TestTree
testMapQuality = testMap "Quality" mapQuality id

testMatch ::
  String ->
  ([MediaType] -> a -> Maybe MediaType) ->
  ([Quality MediaType] -> a) ->
  TestTree
testMatch name match qToI =
  testGroup
    ("match" ++ name)
    [ testProperty "Most specific" $ do
        media <- genConcreteMediaType
        let client =
              qToI $
                map
                  maxQuality
                  [ MediaType "*" "*" Nothing empty,
                    media {subType = "*"},
                    media {parameters = empty},
                    media
                  ]
        return $ match [media] client === Just media,
      testProperty "Nothing" $ do
        client <- listOf1 genConcreteMediaType
        server <- filter (not . flip any client . matches) <$> genServer
        return $ match server (qToI $ map maxQuality client) === Nothing,
      testProperty "Left biased" $ do
        server <- genNubServer
        let client = qToI $ map maxQuality server
        return $ match server client === Just (head server),
      testProperty "Against */*" $ do
        server <- genNubServer
        let stars = "*/*" :: MediaType
        return $
          match server (qToI [maxQuality stars])
            === Just (head server),
      testProperty "Against type/*" $ do
        server <- genNubServer
        let client = qToI [maxQuality (subStarOf $ head server)]
        return $ match server client === Just (head server),
      testQuality match qToI
    ]

testQuality ::
  ([MediaType] -> a -> Maybe MediaType) ->
  ([Quality MediaType] -> a) ->
  TestTree
testQuality match qToI =
  testGroup
    "Quality"
    [ testProperty "Highest quality" $ do
        server <- genServer
        qs <- replicateM (length server) $ choose (1, 1000)
        let client = zipWith Quality server qs
            qmax v q = if qualityValue q > qualityValue v then q else v
        return $
          match server (qToI client)
            === Just (qualityData $ foldr1 qmax client),
      testProperty "Most specific quality" $ do
        (a, b) <- genMatchingPair
        c <- genDiffMediaType a
        let client = qToI [quality a "0.5", maxQuality b, maxQuality c]
        return $ match [a, c] client === Just c,
      testQ0 match qToI
    ]

testQ0 ::
  ([MediaType] -> a -> Maybe MediaType) ->
  ([Quality MediaType] -> a) ->
  TestTree
testQ0 match qToI =
  testGroup
    "q=0"
    [ testProperty "Does not choose a q=0" $ do
        server <- genConcreteMediaType
        return $ match [server] (qToI [minQuality server]) === Nothing,
      testProperty "Does not choose any q=0" $ do
        server <- genServer
        return $ match server (qToI $ map minQuality server) === Nothing,
      testProperty "Does not choose q=0 with less specific type" $ do
        (a, b) <- genMatchingPair
        let client = qToI [minQuality a, maxQuality b]
        return $ match [a] client === Nothing,
      testProperty "Does choose type with q=0 on less specific type" $ do
        (a, b) <- genMatchingPair
        let client = qToI [minQuality b, maxQuality a]
        return $ match [a] client === Just a,
      testProperty "Does not choose q=0 when followed by same type" $ do
        server <- genConcreteMediaType
        let client = qToI [minQuality server, maxQuality server]
        return $ match [server] client === Nothing,
      testProperty "Does not choose q=0 when preceded by same type" $ do
        server <- genConcreteMediaType
        let client = qToI [maxQuality server, minQuality server]
        return $ match [server] client === Nothing
    ]

testMap ::
  String ->
  ([(MediaType, MediaType)] -> a -> Maybe MediaType) ->
  ([Quality MediaType] -> a) ->
  TestTree
testMap name mapf qToI =
  testGroup
    ("map" ++ name)
    [ testProperty "Matches" $ do
        server <- genServer
        qs <- replicateM (length server) $ choose (1, 1000 :: Word16)
        let client = zipWith Quality server qs
            qmax q v = if qualityValue q >= qualityValue v then q else v
            zipped = zip server server
        return $
          mapf zipped (qToI client)
            === Just (qualityData $ foldr1 qmax client),
      testProperty "Nothing" $ do
        (server, client) <- genServerAndClient
        let zipped = map (,"*/*") server
        return $ mapf zipped (qToI $ map maxQuality client) === Nothing
    ]

genServer :: Gen [MediaType]
genServer = listOf1 genConcreteMediaType

genNubServer :: Gen [MediaType]
genNubServer = nubBy (on (==) stripParams) <$> genServer

genServerAndClient :: Gen ([MediaType], [MediaType])
genServerAndClient = do
  server <- genServer
  client <-
    filter (not . flip any server . flip matches)
      <$> listOf1 (genDiffMediaTypesWith genConcreteMediaType server)
  return (server, client)
