------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Accept.Quality
    (
      Quality (..)
    ) where

------------------------------------------------------------------------------
import Network.HTTP.Accept.Match


------------------------------------------------------------------------------
-- | Attaches a quality to another value.
data Quality a = (:!)
    { unwrap  :: a
      -- | Retrieves the quality of this value.
    , quality :: Float
    } deriving (Eq)

instance Show a => Show (Quality a) where
    show (a :! q) = show a ++ ";q=" ++ show q

instance Match a => Match (Quality a) where
    matches (a :! _) (b :! _) = matches a b
    isMoreSpecific (a :! q) (b :! u)
        | q == u    = isMoreSpecific a b
        | otherwise = q > u
    combine (a :! q) (b :! u) = combine a b :! (q * u)

