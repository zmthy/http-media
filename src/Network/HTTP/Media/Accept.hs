------------------------------------------------------------------------------
-- | Defines the 'Accept' type class, designed to unify types on the matching
-- functions in the Media module.
module Network.HTTP.Media.Accept
    ( Accept (..)
    ) where

import qualified Data.CaseInsensitive as CI

import           Data.ByteString      (ByteString)
import           Data.Proxy           (Proxy)


------------------------------------------------------------------------------
-- | Defines methods for a type whose values can be matched against each
-- other in terms of an HTTP Accept-* header.
--
-- This allows functions to work on both the standard Accept header and
-- others such as Accept-Language that still may use quality values.
class Accept a where

    -- | Specifies how to parse an Accept-* header after quality has been
    -- handled.
    parseAccept :: ByteString -> Maybe a

    -- | Evaluates whether either the left argument matches the right one.
    --
    -- This relation must be a total order, where more specific terms on the
    -- left can produce a match, but a less specific term on the left can
    -- never produce a match. For instance, when matching against media types
    -- it is important that if the client asks for a general type then we can
    -- choose a more specific offering from the server, but if a client asks
    -- for a specific type and the server only offers a more general form,
    -- then we cannot generalise. In this case, the server types will be the
    -- left argument, and the client types the right.
    --
    -- For types with no concept of specificity, this operation is just
    -- equality.
    matches :: a -> a -> Bool

    -- | Evaluates whether the left argument is more specific than the right.
    --
    -- This relation must be irreflexive and transitive. For types with no
    -- concept of specificity, this is the empty relation (always false).
    moreSpecificThan :: a -> a -> Bool

    -- | Indicates whether extension parameters are permitted after the weight
    -- parameter when this type appears in an Accept header. Defaults to
    -- false.
    hasExtensionParameters :: Proxy a -> Bool
    hasExtensionParameters _ = False

instance Accept ByteString where
    parseAccept = Just
    matches a b = CI.mk a == CI.mk b
    moreSpecificThan _ _ = False
