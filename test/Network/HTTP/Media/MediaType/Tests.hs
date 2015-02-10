------------------------------------------------------------------------------
module Network.HTTP.Media.MediaType.Tests (tests) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as Map

------------------------------------------------------------------------------
import Control.Monad                     (join, liftM)
import Data.CaseInsensitive              (foldedCase)
import Data.String                       (fromString)
import Data.Maybe                        (isNothing)
import Data.Monoid                       ((<>))
import Distribution.TestSuite.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept
import Network.HTTP.Media.Gen
import Network.HTTP.Media.MediaType          ((/?), (/.))
import Network.HTTP.Media.MediaType.Internal
import Network.HTTP.Media.MediaType.Gen


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
    return $ parseAccept (BS.pack $ show media) == Just media


------------------------------------------------------------------------------
testFromString :: Test
testFromString = testProperty "fromString" $ do
    media <- genMediaType
    return $ media == fromString (show media)


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
        let is n v = (&& media /. foldedCase n == Just v)
        return $ Map.foldrWithKey is True $ parameters media
    , testProperty "Nothing for property it doesn't have" $ do
        media <- genWithParams
        let is n _ = (&& isNothing (stripParams media /. foldedCase n))
        return $ Map.foldrWithKey is True $ parameters media
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
        return $ mostSpecific media anything == media &&
            mostSpecific anything media == media
    , testProperty "With type/*" $ do
        media <- genConcreteMediaType
        let m1 = media { parameters = Map.empty }
            m2 = m1 { subType = "*" }
        return $ mostSpecific m1 m2 == m1 && mostSpecific m2 m1 == m1
    , testProperty "With parameters" $ do
        media  <- genMediaType
        params <- genParameters
        let media'  = media { parameters = params }
            media'' = media { parameters = Map.empty }
        return $ mostSpecific media' media'' == media' &&
            mostSpecific media'' media' == media'
    , testProperty "Different types" $ do
        media  <- genConcreteMediaType
        media' <- genDiffMediaTypeWith genConcreteMediaType media
        return $ mostSpecific media media' == media
    , testProperty "Left biased" $ do
        media  <- genConcreteMediaType
        media' <- genConcreteMediaType
        let media'' = media' { parameters = parameters media }
        return $ mostSpecific media media'' == media &&
            mostSpecific media'' media == media''
    ]


------------------------------------------------------------------------------
testParseAccept :: Test
testParseAccept = testProperty "parseAccept" $ do
    media <- genMediaType
    let main   = mainType media
        sub    = subType media
    params <- renderParameters (parameters media)
    let parsed = parseAccept $ foldedCase (main <> "/" <> sub) <> params
    return $ parsed == Just media


------------------------------------------------------------------------------
-- | Like 'join', but applies the given function to the first argument.
dotJoin :: (a -> a -> b) -> (a -> a) -> a -> b
dotJoin f g a = f (g a) a
