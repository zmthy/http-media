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

    -- | Evaluates whether either the left argument matches the right one
    -- (order may be important). In particular, when using 'matchAccept',
    -- the server option is the first argument and the client option is
    -- the second.
    matches :: a -> a -> Bool

    -- | Evaluates whether the left argument is more specific than the right.
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

