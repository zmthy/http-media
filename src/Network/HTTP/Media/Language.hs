------------------------------------------------------------------------------
-- | Defines the 'Language' accept header with an 'Accept' instance for use in
-- language negotiation.
module Network.HTTP.Media.Language
    ( Language
    , toParts
    ) where

------------------------------------------------------------------------------
import Data.ByteString (ByteString)

------------------------------------------------------------------------------
import Network.HTTP.Media.Language.Internal


------------------------------------------------------------------------------
-- | Converts 'Language' to a list of its language parts. The wildcard
-- produces an empty list.
toParts :: Language -> [ByteString]
toParts (Language l) = l

