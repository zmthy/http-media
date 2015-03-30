{-# LANGUAGE CPP, TupleSections #-}

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
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative  ((<$>))
#endif
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI)
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
genLanguage = Language <$> listOf genCIByteString


------------------------------------------------------------------------------
-- | Generates a Language that does not match everything.
genConcreteLanguage :: Gen Language
genConcreteLanguage = Language <$> listOf1 genCIByteString


------------------------------------------------------------------------------
-- | Generates a different Language to the given one.
genDiffLanguage :: Language -> Gen Language
genDiffLanguage (Language []) = genConcreteLanguage
genDiffLanguage l             = Gen.genDiffWith genLanguage l


------------------------------------------------------------------------------
-- | Generate a Language that has the given language as a prefix.
genMatchingLanguage :: Language -> Gen Language
genMatchingLanguage (Language pre) =
    (Language . (pre ++)) <$> listOf genCIByteString


------------------------------------------------------------------------------
-- | Generate a Language that has the given language as a proper prefix.
genDiffMatchingLanguage :: Language -> Gen Language
genDiffMatchingLanguage (Language pre) =
    (Language . (pre ++)) <$> listOf1 genCIByteString


------------------------------------------------------------------------------
-- | Generate a Language that does not have the given language as a prefix.
genNonMatchingLanguage :: Language -> Gen Language
genNonMatchingLanguage (Language [])        = genConcreteLanguage
genNonMatchingLanguage (Language (pre : _)) = do
    pre' <- genDiffCIByteString pre
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
genCIByteString :: Gen (CI ByteString)
genCIByteString =
    resize 8 $ Gen.genCIByteStringFrom (['a'..'z'] ++ ['A'..'Z'])


------------------------------------------------------------------------------
genDiffCIByteString :: CI ByteString -> Gen (CI ByteString)
genDiffCIByteString = Gen.genDiffWith genCIByteString
