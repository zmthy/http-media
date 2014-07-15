------------------------------------------------------------------------------
-- | Defines the 'Accept' type class, designed to unify types on the matching
-- functions in the Media module.
module Network.HTTP.Media.Accept
    ( Accept (..)
    , mostSpecific
    ) where

------------------------------------------------------------------------------
import Data.ByteString
import Data.ByteString.UTF8 (toString)


------------------------------------------------------------------------------
-- | Defines methods for a type whose values can be matched against each
-- other in terms of an HTTP Accept-* header.
--
-- This allows functions to work on both the standard Accept header and
-- others such as Accept-Language that still may use quality values.
class Show a => Accept a where

    -- | Specifies how to parse an Accept-* header after quality has been
    -- handled.
    parseAccept :: ByteString -> Maybe a

    -- | Specifies how to show an Accept-* header. Defaults to the standard
    -- show method.
    --
    -- Mostly useful just for avoiding quotes when rendering 'ByteString's
    -- with accompanying quality.
    showAccept :: a -> String
    showAccept = show

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

instance Accept ByteString where
    parseAccept = Just
    showAccept = toString
    matches = (==)
    moreSpecificThan _ _ = False


------------------------------------------------------------------------------
-- | Evaluates to whichever argument is more specific. Left biased.
mostSpecific :: Accept a => a -> a -> a
mostSpecific a b
    | b `moreSpecificThan` a = b
    | otherwise              = a

