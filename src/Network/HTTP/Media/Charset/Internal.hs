-- | Defines the 'Charset' accept header with an 'Accept' instance for use in
-- language negotiation.
module Network.HTTP.Media.Charset.Internal
  ( Charset (..),
  )
where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI, original)
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Network.HTTP.Media.Accept (Accept (..))
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Media.Utils (isValidToken)

-- | Suitable for HTTP charset as defined in
-- <https://tools.ietf.org/html/rfc7231#section-5.3.3 RFC7231>.
--
-- Specifically:
--
-- > charset = token / "*"
newtype Charset = Charset (CI ByteString)
  deriving (Eq, Ord)

instance Show Charset where
  show = BS.unpack . renderHeader

instance IsString Charset where
  fromString str =
    flip fromMaybe (parseAccept $ BS.pack str) $
      error $
        "Invalid encoding literal " ++ str

instance Accept Charset where
  parseAccept bs = do
    guard $ isValidToken bs
    return $ Charset (CI.mk bs)

  matches _ (Charset "*") = True
  matches a b = a == b

  moreSpecificThan _ (Charset "*") = True
  moreSpecificThan _ _ = False

instance RenderHeader Charset where
  renderHeader (Charset e) = original e
