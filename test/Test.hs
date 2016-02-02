------------------------------------------------------------------------------
module Main (main) where

------------------------------------------------------------------------------
import Test.Framework (defaultMain, testGroup)

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.Accept.Tests    as Accept
import qualified Network.HTTP.Media.Language.Tests  as Language
import qualified Network.HTTP.Media.MediaType.Tests as MediaType
import qualified Network.HTTP.Media.Tests           as Media


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ testGroup "Accept"    Accept.tests
    , testGroup "Language"  Language.tests
    , testGroup "MediaType" MediaType.tests
    , testGroup "Media"     Media.tests
    ]
