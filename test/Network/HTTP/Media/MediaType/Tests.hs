{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
module Network.HTTP.Media.MediaType.Tests (tests) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Functor                          ((<$>))
#endif

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8                 as BS
import qualified Data.Map                              as Map

------------------------------------------------------------------------------
import           Control.Monad                         (join, liftM)
import           Data.ByteString                       (ByteString)
import           Data.CaseInsensitive                  (foldedCase)
import           Data.Monoid                           ((<>))
import           Data.String                           (fromString)
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.QuickCheck                       (property, (.&&.), (===))
import           Test.QuickCheck.Gen                   (Gen)

------------------------------------------------------------------------------
import           Network.HTTP.Media.Accept
import           Network.HTTP.Media.Gen
import           Network.HTTP.Media.MediaType          ((/.), (/?))
import           Network.HTTP.Media.MediaType.Gen
import           Network.HTTP.Media.MediaType.Internal
import           Network.HTTP.Media.RenderHeader       (renderHeader)


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testEq
    , testShow
    , testFromString
    , testHas
    , testGet
    , testMatches
    , testMoreSpecificThan
    , testMostSpecific
    , testParseAccept
    ]


------------------------------------------------------------------------------
-- Equality is derived, but we test it here to get 100% coverage.
testEq :: Test
testEq = testGroup "Eq"
    [ testProperty "==" $ do
        media <- genMediaType
        return $ media === media
    , testProperty "/=" $ do
        media  <- genMediaType
        media' <- genDiffMediaType media
        return $ media /= media'
    ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testProperty "show" $ do
    media <- genMediaType
    return $ parseAccept (BS.pack $ show media) === Just media


------------------------------------------------------------------------------
testFromString :: Test
testFromString = testProperty "fromString" $ do
    media <- genMediaType
    return $ media === fromString (show media)


------------------------------------------------------------------------------
testHas :: Test
testHas = testGroup "(/?)"
    [ testProperty "True for property it has" $ do
        media <- genWithParams
        return $ all ((media /?) . foldedCase) (Map.keys $ parameters media)
    , testProperty "False for property it doesn't have" $ do
        media <- genWithParams
        return $ all (not . (stripParams media /?) . foldedCase)
            (Map.keys $ parameters media)
    ]


------------------------------------------------------------------------------
testGet :: Test
testGet = testGroup "(/.)"
    [ testProperty "Retrieves property it has" $ do
        media  <- genWithParams
        let is n v = (.&&. media /. foldedCase n === Just v)
        return $ Map.foldrWithKey is (property True) $ parameters media
    , testProperty "Nothing for property it doesn't have" $ do
        media <- genWithParams
        let is n _ = (.&&. stripParams media /. foldedCase n === Nothing)
        return $ Map.foldrWithKey is (property True) $ parameters media
    ]


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Equal values match" $ do
        media <- genMediaType
        return $ matches media media
    , testProperty "Same sub but different main don't match" $ do
        media <- genMaybeSubStar
        main  <- genDiffCIByteString $ mainType media
        return $ not (matches media media { mainType = main }) &&
            not (matches media { mainType = main } media)
    , testProperty "Same main but different sub don't match" $ do
        media <- genConcreteMediaType
        sub   <- genDiffCIByteString $ subType media
        return . not $ matches media media { subType = sub } ||
            matches media { subType = sub } media
    , testProperty "Different parameters don't match" $
        liftM (not . dotJoin matches stripParams) genWithParams
    , testProperty "Missing parameters match" $ do
        media <- genWithParams
        let media' = stripParams media
        return $ matches media media' && not (matches media' media)
    , testGroup "*/*"
        [ testProperty "Matches itself" $ matches anything anything
        , testProperty "Matches anything on the right" $
            liftM (`matches` anything) genMediaType
        , testProperty "Doesn't match more specific on the left" $
            liftM (not . matches anything) genMaybeSubStar
        ]
    , testGroup "type/*"
        [ testProperty "Matches itself" $ liftM (join matches) genSubStar
        , testProperty "Matches on the right" $
            liftM (dotJoin (flip matches) subStarOf) genConcreteMediaType
        , testProperty "Doesn't match on the left" $
            liftM (not . dotJoin matches subStarOf) genConcreteMediaType
        ]
    ]


------------------------------------------------------------------------------
testMoreSpecificThan :: Test
testMoreSpecificThan = testGroup "moreSpecificThan"
    [ testProperty "Against */*" $
        liftM (`moreSpecificThan` anything) genMaybeSubStar
    , testProperty "With */*" $
        liftM (not . moreSpecificThan anything) genMaybeSubStar
    , testProperty "Against type/*" $
        liftM (dotJoin (flip moreSpecificThan) subStarOf) genConcreteMediaType
    , testProperty "With type/*" $
        liftM (not . dotJoin moreSpecificThan subStarOf) genConcreteMediaType
    , testProperty "With parameters" $
        liftM (dotJoin (flip moreSpecificThan) stripParams) genWithParams
    , testProperty "Different types" $ do
        media  <- genWithoutParams
        media' <- genDiffMediaTypeWith genWithoutParams media
        return . not $
            moreSpecificThan media media' || moreSpecificThan media' media
    , testProperty "Different parameters" $ do
        media  <- genWithParams
        params <- genDiffParameters $ parameters media
        return . not $ moreSpecificThan media media { parameters = params }
    ]


------------------------------------------------------------------------------
testMostSpecific :: Test
testMostSpecific = testGroup "mostSpecific"
    [ testProperty "With */*" $ do
        media <- genConcreteMediaType
        return $ mostSpecific media anything === media .&&.
            mostSpecific anything media === media
    , testProperty "With type/*" $ do
        media <- genConcreteMediaType
        let m1 = media { parameters = Map.empty }
            m2 = m1 { subType = "*" }
        return $ mostSpecific m1 m2 === m1 .&&. mostSpecific m2 m1 === m1
    , testProperty "With parameters" $ do
        media  <- genMediaType
        params <- genParameters
        let media'  = media { parameters = params }
            media'' = media { parameters = Map.empty }
        return $ mostSpecific media' media'' === media' .&&.
            mostSpecific media'' media' === media'
    , testProperty "Different types" $ do
        media  <- genConcreteMediaType
        media' <- genDiffMediaTypeWith genConcreteMediaType media
        return $ mostSpecific media media' === media
    , testProperty "Left biased" $ do
        media  <- genConcreteMediaType
        media' <- genConcreteMediaType
        let media'' = media' { parameters = parameters media }
        return $ mostSpecific media media'' === media .&&.
            mostSpecific media'' media === media''
    ]


------------------------------------------------------------------------------
testParseAccept :: Test
testParseAccept = testGroup "parseAccept"
    [ testProperty "Valid parse" $ do
        media <- genMediaType
        let main = mainType media
            sub  = subType media
        params <- renderParameters (parameters media)
        let parsed = parseAccept $ foldedCase (main <> "/" <> sub) <> params
        return $ parsed === Just media
    , testProperty "No sub" $ do
        bs <- genByteString
        return $ (parseAccept bs :: Maybe MediaType) === Nothing
    , testProperty "Empty main" $ do
        sep <- padString "/"
        bs  <- (sep <>) <$> genByteString
        return $ (parseAccept bs :: Maybe MediaType) === Nothing
    , testProperty "Empty sub" $ do
        sep <- padString "/"
        bs  <- (<> sep) <$> genByteString
        return $ (parseAccept bs :: Maybe MediaType) === Nothing
    , testProperty "Empty parameters" $ do
        sep <- padString ";"
        bs  <- renderHeader <$> genWithoutParams
        return $ (parseAccept (bs <> sep) :: Maybe MediaType) === Nothing
    , testProperty "No value" $
        (=== Nothing) <$> genMediaNameAndParams ""
    , testProperty "Empty value" $ do
        eq <- padString "="
        (=== Nothing) <$> genMediaNameAndParams eq
    ]

genMediaNameAndParams :: ByteString -> Gen (Maybe MediaType)
genMediaNameAndParams eq = do
    sep  <- padString ";"
    bs   <- renderHeader <$> genByteString
    name <- genByteString
    ps   <- genMaybeParameters >>= renderParameters
    return $ parseAccept (bs <> sep <> name <> eq <> sep <> ps)


------------------------------------------------------------------------------
-- | Like 'join', but applies the given function to the first argument.
dotJoin :: (a -> a -> b) -> (a -> a) -> a -> b
dotJoin f g a = f (g a) a
