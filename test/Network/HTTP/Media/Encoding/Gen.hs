{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
-- | Contains definitions for generating 'Encoding's.
module Network.HTTP.Media.Encoding.Gen
    ( anything
    , identity
    , genEncoding
    , genConcreteEncoding
    , genDiffEncoding
    , genDiffConcreteEncodings
    ) where

import           Data.ByteString                      (ByteString)
import           Data.CaseInsensitive                 (CI)
import           Test.QuickCheck.Gen

import qualified Network.HTTP.Media.Gen               as Gen
import qualified Network.HTTP.Media.Utils             as Utils

import           Network.HTTP.Media.Encoding.Internal


------------------------------------------------------------------------------
-- | The Encoding that matches anything.
anything :: Encoding
anything = Encoding "*"


------------------------------------------------------------------------------
-- | The default Encoding.
identity :: Encoding
identity = Encoding "identity"


------------------------------------------------------------------------------
-- | Generates any kind of Encoding.
genEncoding :: Gen Encoding
genEncoding = Encoding <$> genToken


------------------------------------------------------------------------------
-- | Generates an Encoding that does not match everything.
genConcreteEncoding :: Gen Encoding
genConcreteEncoding = Gen.genDiffWith genEncoding anything


------------------------------------------------------------------------------
-- | Generates a different Encoding to the given one.
genDiffEncoding :: Encoding -> Gen Encoding
genDiffEncoding = Gen.genDiffWith genEncoding


------------------------------------------------------------------------------
-- | Generates two different concrete Encodings.
genDiffConcreteEncodings :: Gen (Encoding, Encoding)
genDiffConcreteEncodings = do
    enc <- genConcreteEncoding
    (enc,) <$> Gen.genDiffWith genConcreteEncoding enc


------------------------------------------------------------------------------
-- | Generates a valid header token.
genToken :: Gen (CI ByteString)
genToken = Gen.genCIByteStringFrom Utils.tokenChars
