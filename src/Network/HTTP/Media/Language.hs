------------------------------------------------------------------------------
-- | Defines the 'Language' accept header with an 'Accept' instance for use in
-- language negotiation.
module Network.HTTP.Media.Language
    ( Language
    , toByteString
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)

------------------------------------------------------------------------------
import Control.Monad   (guard)
import Data.ByteString (ByteString)
import Data.Functor    ((<$>))
import Data.List       (isPrefixOf)
import Data.Maybe      (fromMaybe)
import Data.String     (IsString (..))

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept (Accept (..))
import Network.HTTP.Media.Utils  (hyphen, isAlpha)


------------------------------------------------------------------------------
-- | Suitable for HTTP language-ranges as defined in
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4 RFC2616>.
--
-- Specifically:
--
-- > language-range = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
data Language
    = Language [ByteString]
    | Any

instance Show Language where
    show = BS.toString . toByteString

instance IsString Language where
    fromString "*" = Any
    fromString str = flip fromMaybe (parseAccept $ BS.fromString str) $
        error $ "Invalid language literal " ++ str

instance Accept Language where
    parseAccept "*" = Just Any
    parseAccept bs = do
        let pieces = BS.split hyphen bs
        guard $ not (null pieces)
        Language <$> mapM check pieces
      where
        check part = do
            let len = BS.length part
            guard $ len >= 1 && len <= 8 && BS.all isAlpha part
            return part

    -- Languages match if the right argument is a prefix of the left.
    matches _            Any          = True
    matches Any          (Language _) = False
    matches (Language a) (Language b) = b `isPrefixOf` a

    -- The left language is more specific than the right if the right
    -- arguments is a strict prefix of the left.
    moreSpecificThan (Language _) Any          = True
    moreSpecificThan (Language a) (Language b) = length a > length b
    moreSpecificThan _            _            = False


------------------------------------------------------------------------------
-- | Converts 'Language' to 'ByteString'.
toByteString :: Language -> ByteString
toByteString Any          = "*"
toByteString (Language l) = BS.intercalate "-" l

