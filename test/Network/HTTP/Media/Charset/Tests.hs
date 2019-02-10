------------------------------------------------------------------------------
module Network.HTTP.Media.Charset.Tests (tests) where

import qualified Data.ByteString.Char8                as BS

import           Control.Monad                        (join)
import           Data.String                          (fromString)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      ((===))

import           Network.HTTP.Media.Accept
import           Network.HTTP.Media.Charset           (Charset)
import           Network.HTTP.Media.Charset.Gen
import           Network.HTTP.Media.RenderHeader


------------------------------------------------------------------------------
tests :: [Test]
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
testEq :: Test
testEq = testGroup "Eq"
    [ testProperty "==" $ do
        enc <- genCharset
        return $ enc === enc
    , testProperty "/=" $ do
        enc  <- genCharset
        enc' <- genDiffCharset enc
        return $ enc /= enc'
    ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testProperty "show" $ do
    enc <- genCharset
    return $ parseAccept (BS.pack $ show enc) === Just enc


------------------------------------------------------------------------------
testFromString :: Test
testFromString = testProperty "fromString" $ do
    enc <- genCharset
    return $ enc === fromString (show enc)


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Equal values match" $
        join matches <$> genCharset
    , testProperty "* matches anything" $
        flip matches anything <$> genCharset
    , testProperty "No concrete encoding matches *" $
        not . matches anything <$> genConcreteCharset
    ]


------------------------------------------------------------------------------
testMoreSpecific :: Test
testMoreSpecific = testGroup "moreSpecificThan"
    [ testProperty "Against *" $
        flip moreSpecificThan anything <$> genConcreteCharset
    , testProperty "With *" $
        not . moreSpecificThan anything <$> genConcreteCharset
    , testProperty "Unrelated encodings" $
        not . uncurry moreSpecificThan <$> genDiffConcreteCharsets
    ]


------------------------------------------------------------------------------
testParseAccept :: Test
testParseAccept = testGroup "parseAccept"
    [ testProperty "Empty" $
        parseAccept "" === (Nothing :: Maybe Charset)
    , testProperty "Wildcard" $
        parseAccept "*" === Just anything
    , testProperty "Valid parse" $ do
        enc <- genCharset
        return $ parseAccept (renderHeader enc) === Just enc
    ]
