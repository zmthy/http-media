{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Contains definitions for generating 'MediaType's.
module Network.HTTP.Accept.MediaType.Gen
    (
      -- * Generating ByteStrings
      genByteString
    , genDiffByteString

      -- * Generating MediaTypes
    , genMediaType
    , genDiffMediaTypes
    , genDiffMediaType
    , genMediaTypeWith
    , noStar
    , mayStar

      -- * Generating Parameters
    , mayParams
    , someParams
    , diffParams

    ) where

------------------------------------------------------------------------------
import Control.Monad (liftM, liftM2)

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
-- | Uses generators to build a new MediaType generator.
genMediaTypeWith :: Gen ByteString
                 -> Gen ByteString
                 -> Gen MediaType
genMediaTypeWith genMain genSub = do
    main   <- genMain
    sub    <- if main == "*" then return "*" else genSub
    params <- mayParams
    return $ MediaType main sub params


------------------------------------------------------------------------------
-- | Generates a random MediaType.
genMediaType :: Gen MediaType
genMediaType = genMediaTypeWith mayStar mayStar


------------------------------------------------------------------------------
-- | Generates a (conservatively) different MediaType to the ones in the given
-- list.
genDiffMediaTypes :: [MediaType] -> Gen MediaType
genDiffMediaTypes media = do
    media' <- genMediaType
    if any (eitherMatches media') media
        then genDiffMediaTypes media
        else return media'
  where
    {-eitherMatches a b = a `matches` b || b `matches` a-}
    eitherMatches = (==)


------------------------------------------------------------------------------
genDiffMediaType :: MediaType -> Gen MediaType
genDiffMediaType = genDiffMediaTypes . (: [])


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

