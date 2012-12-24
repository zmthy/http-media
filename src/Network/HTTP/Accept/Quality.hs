------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Accept.Quality
    (
      Quality
    , (/!)
    ) where

------------------------------------------------------------------------------
import           Data.Word (Word16)


------------------------------------------------------------------------------
-- | Attaches a quality value to another value.
data Quality a = Quality a Word16
    deriving (Eq)

instance Show a => Show (Quality a) where
    show (Quality a q) = show a ++ ";q=" ++ show (fromIntegral q / 1000)


------------------------------------------------------------------------------
-- | Creates a quality value with the given amount.
(/!) :: a -> Float -> Quality a
a /! q = Quality a . min 1000 $ round (q * 1000)

