-----------------------------------------------------------------------------
-- | Common utilities for the Accept library.
module Network.HTTP.Accept.Utils
    (
      breakByte
    , trimBS

    , slash
    , semi
    , comma
    , space
    , equal
    ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char (ord)
import           Data.Word (Word8)


------------------------------------------------------------------------------
-- | Equivalent to 'Data.ByteString.breakByte', but leaves out the byte the
-- string is broken on.
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte w = fmap BS.tail . BS.breakByte w


------------------------------------------------------------------------------
-- | Trims space characters from both ends of a ByteString.
trimBS :: ByteString -> ByteString
trimBS = BS.reverse . dropSpace . BS.reverse . dropSpace
  where
    dropSpace = BS.dropWhile (== space)


------------------------------------------------------------------------------
-- 'ByteString' compatible characters.

slash, semi, comma, space :: Word8
slash = word '/'
semi  = word ';'
comma = word ','
space = word ' '
equal = word '='

-- | Converts a 'Char' into a 'Word8'.
word :: Char -> Word8
word = fromIntegral . ord

