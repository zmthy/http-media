------------------------------------------------------------------------------
-- | Defines the 'RenderHeader' type class, with the 'renderHeader' method.
-- 'renderHeader' can be used to render basic header values (acting as
-- identity on 'ByteString's), but it will also work on lists of quality
-- values, which provides the necessary interface for rendering the full
-- possibilities of Accept headers.
module Network.HTTP.Media.RenderHeader
    ( RenderHeader (..)
    ) where

------------------------------------------------------------------------------
import Data.ByteString (ByteString, intercalate)


------------------------------------------------------------------------------
-- | A class for header values, so they may be rendered to their 'ByteString'
-- representation. Lists of header values and quality-marked header values
-- will render appropriately.
class RenderHeader h where

    -- | Render a header value to a UTF-8 'ByteString'.
    renderHeader :: h -> ByteString

instance RenderHeader ByteString where
    renderHeader = id

instance RenderHeader h => RenderHeader [h] where
    renderHeader = intercalate "," . map renderHeader
