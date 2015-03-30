{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
module Network.HTTP.Media.Language.Tests (tests) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative               ((<$>))
#endif
import Control.Monad                     (join)
import Data.String                       (fromString)
import Distribution.TestSuite.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Media.Language.Gen


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testEq
    , testShow
    , testFromString
    , testMatches
    , testMoreSpecific
    , testMostSpecific
    , testParseAccept
    ]


------------------------------------------------------------------------------
-- Equality is derived, but we test it here to get 100% coverage.
testEq :: Test
testEq = testGroup "Eq"
    [ testProperty "==" $ do
        lang <- genLanguage
        return $ lang == lang
    , testProperty "/=" $ do
        lang  <- genLanguage
        lang' <- genDiffLanguage lang
        return $ lang /= lang'
    ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testProperty "show" $ do
    lang <- genLanguage
    return $ parseAccept (BS.pack $ show lang) == Just lang


------------------------------------------------------------------------------
testFromString :: Test
testFromString = testProperty "fromString" $ do
    lang <- genLanguage
    return $ lang == fromString (show lang)


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Equal values match" $
        join matches <$> genLanguage
    , testProperty "Right prefix matches left" $
        uncurry (flip matches) <$> genMatchingLanguages
    , testProperty "Left prefix does not match right" $
        not . uncurry matches <$> genDiffMatchingLanguages
    , testProperty "* matches anything" $
        flip matches anything <$> genLanguage
    , testProperty "No concrete language matches *" $
        not . matches anything <$> genConcreteLanguage
    ]


------------------------------------------------------------------------------
testMoreSpecific :: Test
testMoreSpecific = testGroup "moreSpecificThan"
    [ testProperty "Against *" $
        flip moreSpecificThan anything <$> genConcreteLanguage
    , testProperty "With *" $
        not . moreSpecificThan anything <$> genLanguage
    , testProperty "Proper prefix lhs" $
        not . uncurry moreSpecificThan <$> genDiffMatchingLanguages
    , testProperty "Proper prefix rhs" $
        uncurry (flip moreSpecificThan) <$> genDiffMatchingLanguages
    , testProperty "Unrelated languages" $
        not . uncurry moreSpecificThan <$> genNonMatchingLanguages
    ]


------------------------------------------------------------------------------
testMostSpecific :: Test
testMostSpecific = testGroup "mostSpecific"
    [ testProperty "With *" $ do
        lang <- genLanguage
        return $ mostSpecific lang anything == lang &&
            mostSpecific anything lang == lang
    , testProperty "Proper prefix" $ do
        (pre, lang) <- genMatchingLanguages
        return $ mostSpecific lang pre == lang &&
            mostSpecific pre lang == lang
    , testProperty "Left biased" $ do
        (lang, lang') <- genNonMatchingLanguages
        return $ mostSpecific lang lang' == lang &&
            mostSpecific lang' lang == lang'
    ]


------------------------------------------------------------------------------
testParseAccept :: Test
testParseAccept = testProperty "parseAccept" $ do
    lang <- genLanguage
    return $ parseAccept (renderHeader lang) == Just lang
