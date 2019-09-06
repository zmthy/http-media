------------------------------------------------------------------------------
module Main (main) where

import           Test.Tasty                         (defaultMain, testGroup)

import qualified Network.HTTP.Media.Accept.Tests    as Accept
import qualified Network.HTTP.Media.Charset.Tests   as Charset
import qualified Network.HTTP.Media.Encoding.Tests  as Encoding
import qualified Network.HTTP.Media.Language.Tests  as Language
import qualified Network.HTTP.Media.MediaType.Tests as MediaType
import qualified Network.HTTP.Media.Tests           as Media


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "http-media"
    [ testGroup "Accept"    Accept.tests
    , testGroup "Charset"   Charset.tests
    , testGroup "Encoding"  Encoding.tests
    , testGroup "Language"  Language.tests
    , testGroup "MediaType" MediaType.tests
    , testGroup "Media"     Media.tests
    ]
