{-| Defines the 'Language' type and functions operating on it, including
    an 'Accept' instance for use in language-negotiation.
-}
module Network.HTTP.Media.Language (
      Language
    , toByteString
    ) where

import Network.HTTP.Media.Language.Internal