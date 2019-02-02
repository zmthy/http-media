------------------------------------------------------------------------------
module Network.HTTP.Media.Encoding.Tests (tests) where

import qualified Data.ByteString.Char8                as BS

import           Control.Monad                        (join)
import           Data.String                          (fromString)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      ((===))

import           Network.HTTP.Media.Accept
import           Network.HTTP.Media.Encoding.Gen
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
        enc <- genEncoding
        return $ enc === enc
    , testProperty "/=" $ do
        enc  <- genEncoding
        enc' <- genDiffEncoding enc
        return $ enc /= enc'
    ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testProperty "show" $ do
    enc <- genEncoding
    return $ parseAccept (BS.pack $ show enc) === Just enc


------------------------------------------------------------------------------
testFromString :: Test
testFromString = testProperty "fromString" $ do
    enc <- genEncoding
    return $ enc === fromString (show enc)


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Equal values match" $
        join matches <$> genEncoding
    , testProperty "* matches anything" $
        flip matches anything <$> genEncoding
    , testProperty "No concrete encoding matches *" $
        not . matches anything <$> genConcreteEncoding
    ]


------------------------------------------------------------------------------
testMoreSpecific :: Test
testMoreSpecific = testGroup "moreSpecificThan"
    [ testProperty "Against *" $
        flip moreSpecificThan anything <$> genConcreteEncoding
    , testProperty "With *" $
        not . moreSpecificThan anything <$> genConcreteEncoding
    , testProperty "Unrelated encodings" $
        not . uncurry moreSpecificThan <$> genDiffConcreteEncodings
    ]


------------------------------------------------------------------------------
testParseAccept :: Test
testParseAccept = testGroup "parseAccept"
    [ testProperty "Empty" $
        parseAccept "" === Just identity
    , testProperty "Wildcard" $
        parseAccept "*" === Just anything
    , testProperty "Valid parse" $ do
        enc <- genEncoding
        return $ parseAccept (renderHeader enc) === Just enc
    ]
