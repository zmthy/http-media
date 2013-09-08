{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Contains definitions for generating 'MediaType's.
module Network.HTTP.Accept.MediaType.Gen
    (
    -- * Anything
      anything

    -- * Generating ByteStrings
    , genByteString
    , genDiffByteString

    -- * Generating MediaTypes
    , genMediaType
    , genConcreteMediaType
    , genDiffMediaTypes
    , genDiffMediaType
    , genDiffConcreteMediaType
    , genMediaTypeWith
    , noStar
    , mayStar

    -- * Generating Parameters
    , mayParams
    , someParams
    , diffParams
    ) where

------------------------------------------------------------------------------
import qualified Data.Map as Map

------------------------------------------------------------------------------
import Control.Monad (join, liftM, liftM2)
import Data.ByteString (ByteString)
import Data.Map (fromList)
import Test.QuickCheck.Gen

------------------------------------------------------------------------------
import Network.HTTP.Accept.Gen
import Network.HTTP.Accept.Match (matches)
import Network.HTTP.Accept.MediaType.Internal


------------------------------------------------------------------------------
-- | Parameter entry for testing.
type ParamEntry = (ByteString, ByteString)


------------------------------------------------------------------------------
anything :: MediaType
anything = MediaType "*" "*" Map.empty


------------------------------------------------------------------------------
-- | Uses generators to build a new MediaType generator.
genMediaTypeWith :: Gen ByteString
                 -> Gen ByteString
                 -> Gen MediaType
genMediaTypeWith genMain genSub = do
    main <- genMain
    if main == "*" then return anything else do
        sub <- genSub
        if sub == "*" then return $ MediaType main sub Map.empty else
            liftM (MediaType main sub) mayParams


------------------------------------------------------------------------------
-- | Generates a random MediaType.
genMediaType :: Gen MediaType
genMediaType = genMediaTypeWith mayStar mayStar


------------------------------------------------------------------------------
-- | Generates a random MediaType with no wildcards.
genConcreteMediaType :: Gen MediaType
genConcreteMediaType = join genMediaTypeWith genByteString


------------------------------------------------------------------------------
-- | Generates a (conservatively) different MediaType to the ones in the given
-- list, using the given generators.
genDiffMediaTypesWith :: Gen ByteString -> Gen ByteString -> [MediaType]
                  -> Gen MediaType
genDiffMediaTypesWith main sub media = do
    media' <- genMediaTypeWith main sub
    if any (eitherMatches media') media
        then genDiffMediaTypes media
        else return media'
  where
    {-eitherMatches a b = a `matches` b || b `matches` a-}
    eitherMatches = (==)


------------------------------------------------------------------------------
-- | Generates a (conservatively) different MediaType to the ones in the given
-- list.
genDiffMediaTypes :: [MediaType] -> Gen MediaType
genDiffMediaTypes = join genDiffMediaTypesWith mayStar


------------------------------------------------------------------------------
-- | Generates a (conservatively) different MediaType to the given one.
genDiffMediaType :: MediaType -> Gen MediaType
genDiffMediaType = genDiffMediaTypes . (: [])


------------------------------------------------------------------------------
-- | Generates a concrete (conservatively) different MediaType to the given
-- one.
genDiffConcreteMediaType :: MediaType -> Gen MediaType
genDiffConcreteMediaType = join genDiffMediaTypesWith genByteString . (: [])


------------------------------------------------------------------------------
-- | An alt generator producing either an alpha ByteString or a star.
mayStar :: Gen ByteString
mayStar = oneof [genByteString, return "*"]


------------------------------------------------------------------------------
-- | An alt generator producing an alpha ByteString.
noStar :: Gen ByteString
noStar = genByteString


------------------------------------------------------------------------------
-- | Reuse for 'mayParams' and 'someParams'.
mkGenParams :: (Gen ParamEntry -> Gen [ParamEntry]) -> Gen Parameters
mkGenParams = liftM fromList .
    ($ liftM2 (,) (genDiffByteString "q") genByteString)


------------------------------------------------------------------------------
-- | Generates some sort of parameters.
mayParams :: Gen Parameters
mayParams = mkGenParams listOf


------------------------------------------------------------------------------
-- | Generates at least one parameter.
someParams :: Gen Parameters
someParams = mkGenParams listOf1


------------------------------------------------------------------------------
-- | Generates a set of parameters that is not a submap of the given
-- parameters (but not necessarily vice versa).
diffParams :: Parameters -> Gen Parameters
diffParams params = do
    params' <- someParams
    if params' == params then diffParams params else return params'

