-----------------------------------------------------------------------------
-- | Common utilities.
module Network.HTTP.Media.Utils
    ( breakChar
    , trimBS

    , validChars
    , isValidChar
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS

------------------------------------------------------------------------------
import Data.ByteString (ByteString)


------------------------------------------------------------------------------
-- | Equivalent to 'Data.ByteString.break' (on equality against the given
-- character), but leaves out the byte that the string is broken on.
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c = fmap BS.tail . BS.break (== c)


------------------------------------------------------------------------------
-- | Trims space characters from both ends of a ByteString.
trimBS :: ByteString -> ByteString
trimBS = BS.reverse . dropSpace . BS.reverse . dropSpace
  where
    dropSpace = BS.dropWhile (== ' ')


------------------------------------------------------------------------------
-- | List of the valid characters for a media-type `reg-name` as per RFC 4288.
validChars :: [Char]
validChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!#$&.+-^_"


------------------------------------------------------------------------------
-- | Evaluates whether the given character is valid in a media type `reg-name`
-- as per RFC 4288.
isValidChar :: Char -> Bool
isValidChar = (`elem` validChars)
