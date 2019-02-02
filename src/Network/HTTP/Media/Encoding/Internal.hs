------------------------------------------------------------------------------
-- | Defines the 'Encoding' accept header with an 'Accept' instance for use in
-- language negotiation.
module Network.HTTP.Media.Encoding.Internal
    ( Encoding (..)
    ) where

import qualified Data.ByteString.Char8           as BS
import qualified Data.CaseInsensitive            as CI

import           Control.Monad                   (guard)
import           Data.ByteString                 (ByteString)
import           Data.CaseInsensitive            (CI, original)
import           Data.Maybe                      (fromMaybe)
import           Data.String                     (IsString (..))

import           Network.HTTP.Media.Accept       (Accept (..))
import           Network.HTTP.Media.RenderHeader (RenderHeader (..))
import           Network.HTTP.Media.Utils        (isValidToken)


------------------------------------------------------------------------------
-- | Suitable for HTTP encoding as defined in
-- <https://tools.ietf.org/html/rfc7231#section-5.3.4 RFC7231>.
--
-- Specifically:
--
-- > codings = content-coding / "identity" / "*"
newtype Encoding = Encoding (CI ByteString)
    deriving (Eq, Ord)

instance Show Encoding where
    show = BS.unpack . renderHeader

instance IsString Encoding where
    fromString str = flip fromMaybe (parseAccept $ BS.pack str) $
        error $ "Invalid encoding literal " ++ str

instance Accept Encoding where
    -- This handles the case where the header value is empty, but it also
    -- allows technically invalid values such as "compress;q=0.8,;q=0.5".
    parseAccept "" = Just $ Encoding "identity"
    parseAccept bs = do
        guard $ isValidToken bs
        return $ Encoding (CI.mk bs)

    matches _ (Encoding "*") = True
    matches a b              = a == b

    moreSpecificThan _ (Encoding "*") = True
    moreSpecificThan _ _              = False

instance RenderHeader Encoding where
    renderHeader (Encoding e) = original e
