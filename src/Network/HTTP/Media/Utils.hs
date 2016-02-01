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
breakChar :: Char -> ByteString -> Maybe (ByteString, ByteString)
breakChar c = safeTail . BS.break (== c)
  where
    safeTail (a, b)
        | BS.null b = Nothing
        | otherwise = Just (a, BS.tail b)


------------------------------------------------------------------------------
-- | Trims tab and space characters from both ends of a ByteString.
trimBS :: ByteString -> ByteString
trimBS = fst . BS.spanEnd isLWS . BS.dropWhile isLWS
  where
    isLWS c = c == ' ' || c == '\t'


------------------------------------------------------------------------------
-- | List of the valid characters for a media-type `reg-name` as per RFC 4288.
validChars :: [Char]
validChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!#$&.+-^_"


------------------------------------------------------------------------------
-- | Evaluates whether the given character is valid in a media type `reg-name`
-- as per RFC 4288.
isValidChar :: Char -> Bool
isValidChar = (`elem` validChars)
