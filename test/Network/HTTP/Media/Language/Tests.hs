module Network.HTTP.Media.Language.Tests (tests) where

------------------------------------------------------------------------------
import Data.Maybe
import Distribution.TestSuite.QuickCheck

import Network.HTTP.Media.Accept
import Network.HTTP.Media.Language
import Network.HTTP.Media.Language.Gen

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testParseShowRoundtrip
        , testMatch
        , testNoMatch
        , testMoreSpecific
        ]


testParseShowRoundtrip :: Test
testParseShowRoundtrip = testProperty "roundtrip" $ do
    lang <- genLanguage
    return $ (fromJust . parseAccept . toByteString) lang == lang

testMatch :: Test
testMatch = testGroup "match language"
    [ testProperty "exact" $ do
        lang <- genLanguage
        return $ matches lang lang
    , testProperty "prefix" $ do
        (short, long) <- genRelatedLanguages
        return $ matches long short
    , testProperty "* matches X" $ do
        lang <- genLanguage
        return $ matches wildcard lang
    , testProperty " X matches *" $ do
        lang <- genLanguage
        return $ matches lang wildcard
    ]

testNoMatch :: Test
testNoMatch = testGroup "don't match language" 
    [ testProperty "unrelated" $ do
        lang <- genLanguage
        lang' <- genDiffLanguage lang
        return . not $ matches lang lang'
    , testProperty "prefix" $ do
        (short, long) <- genRelatedLanguages
        return . not $ matches short long
    ]

testMoreSpecific :: Test
testMoreSpecific = testGroup "more specific language"
    [ testProperty "prefix less specific" $ do
        (short, long) <- genRelatedLanguages
        return $ moreSpecificThan long short && not (moreSpecificThan short long)
    , testProperty "wild less specific" $ do
        lang <- genLanguage
        return $ moreSpecificThan lang wildcard && not (moreSpecificThan wildcard lang)
    ]

