{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
-- | Contains definitions for generating 'Charset's.
module Network.HTTP.Media.Charset.Gen
    ( anything
    , genCharset
    , genConcreteCharset
    , genDiffCharset
    , genDiffConcreteCharsets
    ) where

import           Test.QuickCheck.Gen

import           Network.HTTP.Media.Gen              (genDiffWith, genToken)

import           Network.HTTP.Media.Charset.Internal


------------------------------------------------------------------------------
-- | The Charset that matches anything.
anything :: Charset
anything = Charset "*"


------------------------------------------------------------------------------
-- | Generates any kind of Charset.
genCharset :: Gen Charset
genCharset = Charset <$> genToken


------------------------------------------------------------------------------
-- | Generates an Charset that does not match everything.
genConcreteCharset :: Gen Charset
genConcreteCharset = genDiffWith genCharset anything


------------------------------------------------------------------------------
-- | Generates a different Charset to the given one.
genDiffCharset :: Charset -> Gen Charset
genDiffCharset = genDiffWith genCharset


------------------------------------------------------------------------------
-- | Generates two different concrete Charsets.
genDiffConcreteCharsets :: Gen (Charset, Charset)
genDiffConcreteCharsets = do
    enc <- genConcreteCharset
    (enc,) <$> genDiffWith genConcreteCharset enc
