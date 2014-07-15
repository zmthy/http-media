{-| Defines the 'Language' type and functions operating on it, includign
    an 'Accept' instance for use in language-negotiation.
-}
module Network.HTTP.Media.Language (
      Language
    , toByteString
    ) where

import Data.Word
import Data.String (IsString(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (toString)

import Data.List
import Control.Applicative

import Network.HTTP.Media.Accept


{-| Suitable for HTTP language-ranges as defined in
    <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4 RFC2616>.
    Specifically:

    >language-range  = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
-}
newtype Language = Lang [ByteString]

instance Show Language where
    show (Lang xs) = intercalate "-" $ toString <$> xs

{-| Render a 'Language' as a 'ByteString'. -}
toByteString :: Language -> ByteString
toByteString (Lang xs) = BS.intercalate "-" xs

instance Accept Language where
    parseAccept input = case BS.split _hyphen input_noQ of 
        [] -> Nothing
        xs -> Lang <$> mapM check xs
        where
        check str | 1 <= BS.length str
                    && BS.length str <= 8
                    && BS.all isAlpha str = Just str
        check _ = Nothing
        input_noQ = BS.takeWhile (/= _semicolon) input

    matches (Lang ["*"]) (Lang _) = True
    matches (Lang _) (Lang ["*"]) = True
    --client (language-range) is a prefix of the server option (language-tag)
    matches (Lang s) (Lang c) = c == take (length c) s

    moreSpecificThan (Lang ["*"]) (Lang ["*"]) = False
    moreSpecificThan (Lang ["*"]) (Lang _) = False
    moreSpecificThan (Lang _) (Lang ["*"]) = True
    moreSpecificThan (Lang a) (Lang b) = length a > length b

instance IsString Language where
    fromString "*" = Lang ["*"]
    fromString str = case parseAccept $ fromString str of
        Nothing -> error "string is not a language"
        Just lang -> lang


-- rather than pull in the whole word8 package, just define a few things:
_space, _tab, _hyphen, _semicolon :: Word8
_space = 32
_tab = 9
_hyphen = 45
_semicolon = 59
isAlpha :: Word8 -> Bool
isAlpha c | 65 <= c && c <= 90 = True
isAlpha c | 97 <= c && c <= 122 = True
isAlpha _ = False