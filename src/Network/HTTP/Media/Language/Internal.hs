{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- | Defines the 'Language' accept header with an 'Accept' instance for use in
-- language negotiation.
module Network.HTTP.Media.Language.Internal
    ( Language (..)
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI

------------------------------------------------------------------------------
import Control.Monad        (guard)
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI, original)
import Data.Char            (isAlpha)
#if !MIN_VERSION_base(4, 8, 0)
import Data.Functor         ((<$>))
#endif
import Data.List            (isPrefixOf)
import Data.Maybe           (fromMaybe)
import Data.String          (IsString (..))

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept       (Accept (..))
import Network.HTTP.Media.RenderHeader (RenderHeader (..))


------------------------------------------------------------------------------
-- | Suitable for HTTP language-ranges as defined in
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4 RFC2616>.
--
-- Specifically:
--
-- > language-range = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
newtype Language = Language [CI ByteString]
    deriving (Eq, Ord)

-- Note that internally, Language [] equates to *.

instance Show Language where
    show = BS.unpack . renderHeader

instance IsString Language where
    fromString "*" = Language []
    fromString str = flip fromMaybe (parseAccept $ BS.pack str) $
        error $ "Invalid language literal " ++ str

instance Accept Language where
    parseAccept "*" = Just $ Language []
    parseAccept bs = do
        let pieces = BS.split '-' bs
        guard $ not (null pieces)
        Language <$> mapM check pieces
      where
        check part = do
            let len = BS.length part
            guard $ len >= 1 && len <= 8 && BS.all isAlpha part
            return (CI.mk part)

    -- Languages match if the right argument is a prefix of the left.
    matches (Language a) (Language b)  = b `isPrefixOf` a

    -- The left language is more specific than the right if the right
    -- arguments is a strict prefix of the left.
    moreSpecificThan (Language a) (Language b) =
        b `isPrefixOf` a && length a > length b

instance RenderHeader Language where
    renderHeader (Language []) = "*"
    renderHeader (Language l)  = BS.intercalate "-" (map original l)
