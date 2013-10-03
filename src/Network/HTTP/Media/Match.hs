------------------------------------------------------------------------------
-- | Defines the 'Match' type class, designed to unify types on the
-- matching functions in the Media module.
module Network.HTTP.Media.Match
    (
      Match (..)
    , mostSpecific
    ) where

------------------------------------------------------------------------------
import Data.ByteString


------------------------------------------------------------------------------
-- | Defines methods for a type whose values can be matched against each
-- other in terms of a HTTP media header.
--
-- This allows functions to work on both the standard Accept header and
-- others such as Accept-Language that still may use quality values.
class Match a where

    -- | Evaluates whether either the left argument matches the right one
    -- (order may be important).
    matches :: a -> a -> Bool

    -- | Evaluates whether the left argument is more specific than the right.
    moreSpecificThan :: a -> a -> Bool

instance Match ByteString where
    matches = (==)
    moreSpecificThan _ _ = False


------------------------------------------------------------------------------
-- | Evaluates to whichever argument is more specific. Left biased.
mostSpecific :: Match a => a -> a -> a
mostSpecific a b
    | b `moreSpecificThan` a = b
    | otherwise              = a

