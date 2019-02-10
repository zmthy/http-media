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

import           Test.QuickCheck.Gen

import           Network.HTTP.Media.Encoding.Internal
import           Network.HTTP.Media.Gen               (genDiffWith, genToken)


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
genConcreteEncoding = genDiffWith genEncoding anything


------------------------------------------------------------------------------
-- | Generates a different Encoding to the given one.
genDiffEncoding :: Encoding -> Gen Encoding
genDiffEncoding = genDiffWith genEncoding


------------------------------------------------------------------------------
-- | Generates two different concrete Encodings.
genDiffConcreteEncodings :: Gen (Encoding, Encoding)
genDiffConcreteEncodings = do
    enc <- genConcreteEncoding
    (enc,) <$> genDiffWith genConcreteEncoding enc
