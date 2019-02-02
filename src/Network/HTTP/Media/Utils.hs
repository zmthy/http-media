-----------------------------------------------------------------------------
-- | Common utilities.
module Network.HTTP.Media.Utils
    ( breakChar
    , trimBS

    , mediaChars
    , isMediaChar

    , tokenChars
    , isTokenChar
    , isValidToken
    ) where

import qualified Data.ByteString.Char8 as BS

import           Data.ByteString       (ByteString)
import           Data.Char             (isControl)


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
mediaChars :: [Char]
mediaChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!#$&.+-^_"


------------------------------------------------------------------------------
-- | Evaluates whether the given character is valid in a media type `reg-name`
-- as per RFC 4288.
isMediaChar :: Char -> Bool
isMediaChar = (`elem` mediaChars)


------------------------------------------------------------------------------
-- | Evaluates whether the given character is valid in an HTTP header token as
-- per RFC 2616.
isTokenChar :: Char -> Bool
isTokenChar = (||) <$> not . isControl <*> (`notElem` separators)
  where
    separators = [ '(', ')', '<', '>', '@', ',', ';', ':', '\\'
                 , '"', '/', '[', ']', '?', '=', '{', '}', ' '
                 ]


------------------------------------------------------------------------------
-- | HTTP header token characters as per RFC 2616.
tokenChars :: [Char]
tokenChars = filter isTokenChar ['\0'..'\127']


------------------------------------------------------------------------------
-- | Evaluates whether the given ASCII string is valid as an HTTP header token
-- as per RFC 2616.
isValidToken :: ByteString -> Bool
isValidToken = (&&) <$> not . BS.null <*> BS.all isTokenChar
