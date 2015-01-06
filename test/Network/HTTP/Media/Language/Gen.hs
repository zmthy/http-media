{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
-- | Contains definitions for generating 'Language's.
module Network.HTTP.Media.Language.Gen
    (
    -- * Generating Languages
      anything
    , genLanguage
    , genConcreteLanguage
    , genDiffLanguage
    , genMatchingLanguage
    , genDiffMatchingLanguage
    , genNonMatchingLanguage
    , genMatchingLanguages
    , genDiffMatchingLanguages
    , genNonMatchingLanguages
    ) where

------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Data.ByteString     (ByteString)
import Test.QuickCheck.Gen

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.Gen as Gen

------------------------------------------------------------------------------
import Network.HTTP.Media.Language.Internal


------------------------------------------------------------------------------
-- | The Language that matches anything.
anything :: Language
anything = Language []


------------------------------------------------------------------------------
-- | Generates any kind of Language.
genLanguage :: Gen Language
genLanguage = Language <$> listOf genByteString


------------------------------------------------------------------------------
-- | Generates a Language that does not match everything.
genConcreteLanguage :: Gen Language
genConcreteLanguage = Language <$> listOf1 genByteString


------------------------------------------------------------------------------
-- | Generates a different Language to the given one.
genDiffLanguage :: Language -> Gen Language
genDiffLanguage (Language []) = genConcreteLanguage
genDiffLanguage lang           = do
    lang' <- genLanguage
    if lang == lang' then genDiffLanguage lang else return lang'


------------------------------------------------------------------------------
-- | Generate a Language that has the given language as a prefix.
genMatchingLanguage :: Language -> Gen Language
genMatchingLanguage (Language pre) =
    (Language . (pre ++)) <$> listOf genByteString


------------------------------------------------------------------------------
-- | Generate a Language that has the given language as a proper prefix.
genDiffMatchingLanguage :: Language -> Gen Language
genDiffMatchingLanguage (Language pre) =
    (Language . (pre ++)) <$> listOf1 genByteString


------------------------------------------------------------------------------
-- | Generate a Language that does not have the given language as a prefix.
genNonMatchingLanguage :: Language -> Gen Language
genNonMatchingLanguage (Language [])        = genConcreteLanguage
genNonMatchingLanguage (Language (pre : _)) = do
    pre' <- genDiffByteString pre
    genMatchingLanguage $ Language [pre']


------------------------------------------------------------------------------
-- | A private definition for generating pairs of languagues.
genLanguages :: (Language -> Gen Language) -> Gen (Language, Language)
genLanguages gen = do
    pre <- genLanguage
    (pre,) <$> gen pre


------------------------------------------------------------------------------
-- | Generate two languages, the first of which is a prefix of the second.
genMatchingLanguages :: Gen (Language, Language)
genMatchingLanguages = genLanguages genMatchingLanguage


------------------------------------------------------------------------------
-- | Generate two languages, the first of which is a proper prefix of the
-- second.
genDiffMatchingLanguages :: Gen (Language, Language)
genDiffMatchingLanguages = genLanguages genDiffMatchingLanguage


------------------------------------------------------------------------------
-- | Generate two languages, the first of which is not a prefix of the second.
genNonMatchingLanguages :: Gen (Language, Language)
genNonMatchingLanguages = do
    pre <- genConcreteLanguage
    (pre,) <$> genNonMatchingLanguage pre


------------------------------------------------------------------------------
genByteString :: Gen ByteString
genByteString = resize 8 $ Gen.genByteStringFrom ([65..90] ++ [97..122])


------------------------------------------------------------------------------
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString = Gen.genDiffByteStringWith genByteString
