------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Accept.Quality
    (
      Quality (..)
    , matches
    ) where

------------------------------------------------------------------------------
import qualified Network.HTTP.Accept.Match as Match


------------------------------------------------------------------------------
-- | Attaches a quality to another value.
data Quality a = (:!)
    { unwrap  :: a
      -- | Retrieves the quality of this value.
    , quality :: Float
    } deriving (Eq)

instance Show a => Show (Quality a) where
    show (a :! q) = show a ++ ";q=" ++ show q


matches :: Match.Match a => a -> Quality a -> Bool
matches a (b :! _) = a `Match.matches` b

