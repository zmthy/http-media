------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Accept.Quality
    (
      Quality (..)
    , unwrap
    ) where

------------------------------------------------------------------------------
import Network.HTTP.Accept.Match


------------------------------------------------------------------------------
-- | Attaches a quality value to another value.
data Quality a = a :! Float
    deriving (Eq)

instance Show a => Show (Quality a) where
    show (a :! q) = show a ++ ";q=" ++ show q

instance Match a => Match (Quality a) where
    matches (a :! _) (b :! _) = matches a b
    isMoreSpecific (a :! q) (b :! u)
        | q == u    = isMoreSpecific a b
        | otherwise = q > u
    combine (a :! q) (b :! u) = combine a b :! (q * u)


------------------------------------------------------------------------------
-- | Removes the quality value from the underlying expression.
unwrap :: Quality a -> a
unwrap (a :! _) = a
