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
    , genDiffMediaType
    , genMediaTypeAndLessParameters

      -- * Generating Parameters
    , genParameters
    , addParameters

    ) where

------------------------------------------------------------------------------
import Control.Monad (liftM)

import Data.ByteString (ByteString, pack)
import Data.List (nubBy)

import Test.QuickCheck.Gen

------------------------------------------------------------------------------
import Network.HTTP.Accept.MediaType


------------------------------------------------------------------------------
-- | Branching generator, used for 'genMediaTypeWith.'
type GenAlt a = MediaType -> (a -> Gen MediaType) -> Gen MediaType


------------------------------------------------------------------------------
-- | A parameter map.
type Parameters = [(ByteString, ByteString)]


------------------------------------------------------------------------------
-- | Produces a ByteString of random alpha characters.
genByteString :: Gen ByteString
genByteString = liftM pack $
    listOf1 (oneof [choose (65, 90), choose (97, 122)])


------------------------------------------------------------------------------
-- | Produces a random ByteString different to the given one.
genDiffByteString :: ByteString -> Gen ByteString
genDiffByteString bs = do
    gbs <- genByteString
    if bs == gbs then genDiffByteString bs else return gbs


------------------------------------------------------------------------------
-- | Uses GenAlts to build a new MediaType generator.
genMediaTypeWith :: GenAlt ByteString
                 -> GenAlt ByteString
                 -> Gen Parameters
                 -> Gen MediaType
genMediaTypeWith m s p = anything `m` \main -> (main // "*") `s` \sub ->
    liftM (addParameters (main // sub)) p


------------------------------------------------------------------------------
-- | An alt generator producing either an alpha ByteString or a star.
genAltMaybeStar :: GenAlt ByteString
genAltMaybeStar a b = do
    value <- oneof [genByteString, return "*"]
    if value == "*" then return a else b value


------------------------------------------------------------------------------
-- | An alt generator producing an alpha ByteString.
genAltNoStar :: GenAlt ByteString
genAltNoStar = const (genByteString >>=)


------------------------------------------------------------------------------
-- | Generates either a list of parameters or an empty list.
genMaybeParameters :: Gen Parameters
genMaybeParameters = oneof [genParameters, return []]


------------------------------------------------------------------------------
-- | Generates a random MediaType.
genMediaType :: Gen MediaType
genMediaType =
    genMediaTypeWith genAltMaybeStar genAltMaybeStar genMaybeParameters


------------------------------------------------------------------------------
-- | Produces a MediaType with at least a different main type than the one
-- given.
genDiffMediaType :: MediaType -> Gen MediaType
genDiffMediaType media = if mainType media == "*"
    then genMediaTypeWith genAltNoStar genAltMaybeStar genMaybeParameters
    else do
        gmedia <- genMediaType
        if gmedia == media then genDiffMediaType media else return gmedia


------------------------------------------------------------------------------
-- | Generates a list of MediaType parameters.
genParameters :: Gen Parameters
genParameters = liftM (nubBy $ (. fst) . (==) . fst) . listOf1 $ do
    name <- genByteString
    value <- genByteString
    return (name, value)


------------------------------------------------------------------------------
-- | Adds the parameters to the MediaType.
addParameters :: MediaType -> [(ByteString, ByteString)] -> MediaType
addParameters = foldr (flip (/:))


-----------------------------------------------------------------------------
genMediaTypeAndLessParameters :: Gen (MediaType, MediaType)
genMediaTypeAndLessParameters = do
    add     <- liftM addParameters genMediaType
    params1 <- genParameters
    params2 <- liftM (map fst . filter snd . zip params1) $
            mapM (const $ choose (True, False)) params1
    return (add params1, add params2)


