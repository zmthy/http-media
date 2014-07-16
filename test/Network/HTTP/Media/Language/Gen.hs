------------------------------------------------------------------------------
-- | Contains definitions for generating 'Language's.
module Network.HTTP.Media.Language.Gen
    ( genLanguage
    , genDiffLanguage
    , genRelatedLanguages
    , wildcard
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.ByteString     (ByteString, pack)
import Network.HTTP.Media.Language.Internal
import Test.QuickCheck.Gen


genLanguage :: Gen Language
genLanguage = Lang <$> resize 8 (listOf1 genByteString)

genDiffLanguage :: Language -> Gen Language
genDiffLanguage lang = do
	lang' <- genLanguage
	if lang == lang' then genDiffLanguage lang else return lang'

-- |Generat two languages, the first of which is a prefix of the second.
genRelatedLanguages :: Gen (Language, Language)
genRelatedLanguages = do
	pre <- resize 7 $ listOf1 genByteString
	post <- resize (8 - length pre) $ listOf1 genByteString
	return (Lang pre, Lang $ pre ++ post)


wildcard :: Language
wildcard = Lang ["*"]

--TODO generate languages related by prefix
--TODO generate unrelated languages

------------------------------------------------------------------------------
-- | Produces a ByteString of random alpha characters.
genByteString :: Gen ByteString
genByteString = liftM pack $ listOf1 (oneof $ map return validChars)
  where
    validChars = [65..90] ++ [97..122]

