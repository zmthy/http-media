{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Contains definitions for generating 'ByteString's and 'MediaType's.
module Network.HTTP.Accept.MediaType.Gen
    (
      -- * Generating ByteStrings
      genByteString
    , genDiffByteString

      -- * Generating MediaTypes
    , genMediaType
    , genMediaTypeWith
    , noStar
    , mayStar

      -- * Generating Parameters
    , mayParams
    , diffParams
    , moreParams

    ) where

------------------------------------------------------------------------------
import Control.Monad (liftM, liftM2)

import Data.ByteString (ByteString, pack)
import Data.Map (Map, fromList, isSubmapOf, union)

import Test.QuickCheck.Gen

------------------------------------------------------------------------------
import Network.HTTP.Accept.MediaType


------------------------------------------------------------------------------
-- | Produces a ByteString of random alpha characters.
genByteString :: Gen ByteString
genByteString = liftM pack $
    listOf1 (oneof [choose (65, 90), choose (97, 122)])


------------------------------------------------------------------------------
-- | Produces a random ByteString different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString bs = do
    bs' <- genByteString
    if bs == bs' then genDiffByteString bs else return bs'


------------------------------------------------------------------------------
-- Parameters types.
type ParamEntry = (ByteString, ByteString)
type Parameters = Map ByteString ByteString


------------------------------------------------------------------------------
-- | Branching generator, used for 'genMediaTypeWith.'
type GenAlt a = MediaType -> (a -> Gen MediaType) -> Gen MediaType


------------------------------------------------------------------------------
-- | Uses GenAlts to build a new MediaType generator.
genMediaTypeWith :: GenAlt ByteString
                 -> GenAlt ByteString
                 -> Gen MediaType
genMediaTypeWith m s = anything `m` \main -> (main // "*") `s` \sub -> do
    params <- mayParams
    return (main // sub) { parameters = params }


------------------------------------------------------------------------------
-- | Generates a random MediaType.
genMediaType :: Gen MediaType
genMediaType = genMediaTypeWith mayStar mayStar


------------------------------------------------------------------------------
-- | An alt generator producing either an alpha ByteString or a star.
--
mayStar :: GenAlt ByteString
mayStar a b = do
    value <- oneof [genByteString, return "*"]
    if value == "*" then return a else b value


------------------------------------------------------------------------------
-- | An alt generator producing an alpha ByteString.
noStar :: GenAlt ByteString
noStar = const (genByteString >>=)


------------------------------------------------------------------------------
-- | Reuse for 'mayParams' and 'someParams'.
mkGenParams :: (Gen ParamEntry -> Gen [ParamEntry]) -> Gen Parameters
mkGenParams = liftM fromList . ($ liftM2 (,) genByteString genByteString)


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
    if params' `isSubmapOf` params then diffParams params else return params'


------------------------------------------------------------------------------
-- | Generates a supermap of the given parameters.
moreParams :: Parameters -> Gen Parameters
moreParams params = do
    params' <- liftM (union params) someParams
    if params' == params then moreParams params else return params'

