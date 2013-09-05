------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Accept.Quality
    (
      Quality (..)
    , unwrap
    , quality
    ) where

------------------------------------------------------------------------------
{-import qualified Network.HTTP.Accept.Match as Match-}


------------------------------------------------------------------------------
-- | Attaches a quality to another value.
data Quality a = Quality a Float
    deriving (Eq)

instance Show a => Show (Quality a) where
    show (Quality a q) = show a ++ ";q=" ++ show q


------------------------------------------------------------------------------
-- | Retrieves the underlying value.
unwrap :: Quality a -> a
unwrap (Quality a _) = a


------------------------------------------------------------------------------
-- | Retrieves the quality number.
quality :: Quality a -> Float
quality (Quality _ q) = q

