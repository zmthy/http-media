------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Media.Quality
    ( Quality (..)
    , maxQuality
    , minQuality
    , readQ
    ) where

------------------------------------------------------------------------------
import Data.Maybe (listToMaybe)
import Data.Word  (Word16)


------------------------------------------------------------------------------
-- | Attaches a quality value to data.
data Quality a = Quality
    { qualityData  :: a
    , qualityValue :: Word16
    } deriving (Eq)

instance Show a => Show (Quality a) where
    show (Quality a q) = show a ++ ";q=" ++ showQ q


------------------------------------------------------------------------------
-- | Attaches the quality value '1'.
maxQuality :: a -> Quality a
maxQuality = flip Quality 1000


------------------------------------------------------------------------------
-- | Attaches the quality value '0'.
minQuality :: a -> Quality a
minQuality = flip Quality 0


------------------------------------------------------------------------------
-- | Converts the integral value into its standard quality representation.
showQ :: Word16 -> String
showQ 1000 = "1"
showQ 0    = "0"
showQ q    = '0' : '.' : let s = show q in replicate (3 - length s) '0' ++ s


------------------------------------------------------------------------------
-- | Reads the standard quality representation into an integral value.
readQ :: String -> Maybe Word16
readQ "1" = Just 1000
readQ "0" = Just 0
readQ ('1' : '.' : t)
    | length t <= 3 && all (== '0') t = Just 1000
    | otherwise                       = Nothing
readQ ('0' : '.' : t)
    | length t <= 3 = fmap fst . listToMaybe . filter (null . snd) . reads $
        t ++ replicate (3 - length t) '0'
    | otherwise     = Nothing
readQ _   = Nothing

