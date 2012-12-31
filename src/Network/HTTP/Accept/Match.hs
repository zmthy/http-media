------------------------------------------------------------------------------
-- | Defines the 'Match' type class, designed to unify types on the
-- matching functions in the Accept module.
module Network.HTTP.Accept.Match
    (
      Match (..)
    , moreSpecific
    ) where

------------------------------------------------------------------------------
import Data.ByteString


------------------------------------------------------------------------------
class Match a where
    -- | Evaluates whether either argument matches the other.
    matches :: a -> a -> Bool
    -- | Evaluates whether the left argument is more specific than the right.
    isMoreSpecific :: a -> a -> Bool
    -- | Combines the two arguments into a single value.
    combine :: a -> a -> a

instance Match ByteString where
    matches = (==)
    isMoreSpecific _ _ = False
    combine a _ = a


------------------------------------------------------------------------------
-- | Evaluates to whichever argument is more specific, choosing the left
-- argument if neither is more specific than the other.
moreSpecific :: Match a => a -> a -> a
moreSpecific a b
    | isMoreSpecific b a = b
    | otherwise          = a

