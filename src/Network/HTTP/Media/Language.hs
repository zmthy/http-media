------------------------------------------------------------------------------
-- | Defines the 'Language' accept header with an 'Accept' instance for use in
-- language negotiation.
module Network.HTTP.Media.Language
    ( Language
    , toParts
    ) where

------------------------------------------------------------------------------
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI)

------------------------------------------------------------------------------
import Network.HTTP.Media.Language.Internal


------------------------------------------------------------------------------
-- | Converts 'Language' to a list of its language parts. The wildcard
-- produces an empty list.
toParts :: Language -> [CI ByteString]
toParts (Language l) = l
