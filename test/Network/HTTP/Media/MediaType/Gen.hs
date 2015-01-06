------------------------------------------------------------------------------
-- | Contains definitions for generating 'MediaType's.
module Network.HTTP.Media.MediaType.Gen
    (
    -- * Generating MediaTypes
      anything
    , genMediaType
    , genSubStar
    , genMaybeSubStar
    , subStarOf
    , genConcreteMediaType
    , genWithoutParams
    , genWithParams
    , stripParams
    , genDiffMediaTypesWith
    , genDiffMediaTypeWith
    , genDiffMediaTypes
    , genDiffMediaType

    -- * Generating Parameters
    , genParameters
    , genMaybeParameters
    , genDiffParameters
    ) where

------------------------------------------------------------------------------
import qualified Data.Map as Map

------------------------------------------------------------------------------
import Control.Monad       (liftM, liftM2)
import Data.ByteString     (ByteString)
import Data.Map            (fromList)
import Test.QuickCheck.Gen

------------------------------------------------------------------------------
import Network.HTTP.Media.Gen
import Network.HTTP.Media.MediaType.Internal


------------------------------------------------------------------------------
-- | Parameter entry for testing.
type ParamEntry = (ByteString, ByteString)


------------------------------------------------------------------------------
-- | The MediaType that matches anything.
anything :: MediaType
anything = MediaType "*" "*" Map.empty


------------------------------------------------------------------------------
-- | Generates any kind of MediaType.
genMediaType :: Gen MediaType
genMediaType = oneof [return anything, genSubStar, genConcreteMediaType]


------------------------------------------------------------------------------
-- | Generates a MediaType with just a concrete main type.
genSubStar :: Gen MediaType
genSubStar = do
    main <- genByteString
    return $ MediaType main "*" Map.empty


------------------------------------------------------------------------------
-- | Generates a MediaType whose sub type might be *.
genMaybeSubStar :: Gen MediaType
genMaybeSubStar = oneof [genSubStar, genConcreteMediaType]


------------------------------------------------------------------------------
-- | Strips the sub type and parameters from a MediaType.
subStarOf :: MediaType -> MediaType
subStarOf media = media { subType = "*", parameters = Map.empty }


------------------------------------------------------------------------------
-- | Generates a concrete MediaType which may have parameters.
genConcreteMediaType :: Gen MediaType
genConcreteMediaType = do
    main <- genByteString
    sub  <- genByteString
    params <- oneof [return Map.empty, genParameters]
    return $ MediaType main sub params


------------------------------------------------------------------------------
-- | Generates a concrete MediaType with no parameters.
genWithoutParams :: Gen MediaType
genWithoutParams = do
    main <- genByteString
    sub  <- genByteString
    return $ MediaType main sub Map.empty


------------------------------------------------------------------------------
-- | Generates a MediaType with at least one parameter.
genWithParams :: Gen MediaType
genWithParams = do
    main   <- genByteString
    sub    <- genByteString
    params <- genParameters
    return $ MediaType main sub params


------------------------------------------------------------------------------
-- | Strips the parameters from the given MediaType.
stripParams :: MediaType -> MediaType
stripParams media = media { parameters = Map.empty }


------------------------------------------------------------------------------
-- | Generates a different MediaType to the ones in the given list, using the
-- given generator.
genDiffMediaTypesWith :: Gen MediaType -> [MediaType] -> Gen MediaType
genDiffMediaTypesWith gen media = do
    media' <- gen
    if media' `elem` media
        then genDiffMediaTypesWith gen media
        else return media'


------------------------------------------------------------------------------
-- | Generates a different MediaType to the given one, using the given
-- generator.
genDiffMediaTypeWith :: Gen MediaType -> MediaType -> Gen MediaType
genDiffMediaTypeWith gen = genDiffMediaTypesWith gen . (: [])


------------------------------------------------------------------------------
-- | Generates a  different MediaType to the ones in the given list.
genDiffMediaTypes :: [MediaType] -> Gen MediaType
genDiffMediaTypes = genDiffMediaTypesWith genMediaType


------------------------------------------------------------------------------
-- | Generates a different MediaType to the given one.
genDiffMediaType :: MediaType -> Gen MediaType
genDiffMediaType = genDiffMediaTypes . (: [])


------------------------------------------------------------------------------
-- | Reuse for 'mayParams' and 'someParams'.
mkGenParams :: (Gen ParamEntry -> Gen [ParamEntry]) -> Gen Parameters
mkGenParams = liftM fromList .
    ($ liftM2 (,) (genDiffByteString "q") genByteString)


------------------------------------------------------------------------------
-- | Generates some sort of parameters.
genMaybeParameters :: Gen Parameters
genMaybeParameters = mkGenParams listOf


------------------------------------------------------------------------------
-- | Generates at least one parameter.
genParameters :: Gen Parameters
genParameters = mkGenParams listOf1


------------------------------------------------------------------------------
-- | Generates a set of parameters that is not a submap of the given
-- parameters (but not necessarily vice versa).
genDiffParameters :: Parameters -> Gen Parameters
genDiffParameters params = do
    params' <- genParameters
    if params' `Map.isSubmapOf` params
        then genDiffParameters params
        else return params'
