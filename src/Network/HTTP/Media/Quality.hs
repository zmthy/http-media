------------------------------------------------------------------------------
-- | Defines the quality value data type.
module Network.HTTP.Media.Quality
    ( Quality (..)
    , maxQuality
    , minQuality
    , showQ
    , readQ
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS

------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Data.Char       (isDigit)
import Data.List       (dropWhileEnd)
import Data.Monoid     ((<>))
import Data.Word       (Word16)

------------------------------------------------------------------------------
import Network.HTTP.Media.RenderHeader (RenderHeader (..))

------------------------------------------------------------------------------
-- | Attaches a quality value to data.
data Quality a = Quality
    { qualityData  :: a
    , qualityValue :: Word16
    } deriving (Eq)

instance RenderHeader a => Show (Quality a) where
    show = BS.unpack . renderHeader

instance RenderHeader h => RenderHeader (Quality h) where
    renderHeader (Quality a q) = renderHeader a <> ";q=" <> showQ q


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
showQ :: Word16 -> ByteString
showQ 1000 = "1"
showQ 0    = "0"
showQ q    = "0." <> BS.replicate (3 - length s) '0' <> b
  where
    s = show q
    b = BS.pack (dropWhileEnd (== '0') s)


------------------------------------------------------------------------------
-- | Reads the standard quality representation into an integral value.
readQ :: ByteString -> Maybe Word16
readQ bs
    | BS.null bs = Nothing
    | h == '1'   = read1 t
    | h == '0'   = read0 t
    | otherwise  = Nothing
  where
    h = BS.head bs
    t = BS.tail bs

read1 :: ByteString -> Maybe Word16
read1 bs
    | BS.null bs || h == '.' && BS.length t < 4 && BS.all (== '0') t
                = Just 1000
    | otherwise = Nothing
  where
    h = BS.head bs
    t = BS.tail bs

read0 :: ByteString -> Maybe Word16
read0 bs
    | BS.null bs = Just 0
    | h == '.' && BS.length t < 4 && BS.all isDigit t
                = Just (toWord (t <> BS.replicate (3 - BS.length t) '0'))
    | otherwise = Nothing
  where
    h = BS.head bs
    t = BS.tail bs
    toWord = read . BS.unpack
