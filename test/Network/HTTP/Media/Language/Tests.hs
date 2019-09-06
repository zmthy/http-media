------------------------------------------------------------------------------
module Network.HTTP.Media.Language.Tests (tests) where

import qualified Data.ByteString.Char8                as BS

import           Control.Monad                        (join)
import           Data.Monoid                          ((<>))
import           Data.String                          (fromString)
import           Test.Tasty                           (TestTree, testGroup)
import           Test.Tasty.QuickCheck                (testProperty)
import           Test.QuickCheck                      ((===))

import           Network.HTTP.Media.Accept
import           Network.HTTP.Media.Language
import           Network.HTTP.Media.Language.Gen
import           Network.HTTP.Media.RenderHeader


------------------------------------------------------------------------------
tests :: [TestTree]
tests =
    [ testEq
    , testShow
    , testFromString
    , testMatches
    , testMoreSpecific
    , testParseAccept
    ]


------------------------------------------------------------------------------
-- Equality is derived, but we test it here to get 100% coverage.
testEq :: TestTree
testEq = testGroup "Eq"
    [ testProperty "==" $ do
        lang <- genLanguage
        return $ lang === lang
    , testProperty "/=" $ do
        lang  <- genLanguage
        lang' <- genDiffLanguage lang
        return $ lang /= lang'
    ]


------------------------------------------------------------------------------
testShow :: TestTree
testShow = testProperty "show" $ do
    lang <- genLanguage
    return $ parseAccept (BS.pack $ show lang) === Just lang


------------------------------------------------------------------------------
testFromString :: TestTree
testFromString = testProperty "fromString" $ do
    lang <- genLanguage
    return $ lang === fromString (show lang)


------------------------------------------------------------------------------
testMatches :: TestTree
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
testMoreSpecific :: TestTree
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
testParseAccept :: TestTree
testParseAccept = testGroup "parseAccept"
    [ testProperty "Valid parse"$ do
        lang <- genLanguage
        return $ parseAccept (renderHeader lang) === Just lang
    , testProperty "Trailing hyphen" $ do
        bs <- renderHeader <$> genLanguage
        return $ (parseAccept $ bs <> "-" :: Maybe Language) === Nothing
    ]
