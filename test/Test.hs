------------------------------------------------------------------------------
module Main (main) where

------------------------------------------------------------------------------
import Test.Framework (defaultMain, testGroup)

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.Tests           as Media
import qualified Network.HTTP.Media.Accept.Tests    as Accept
import qualified Network.HTTP.Media.MediaType.Tests as MediaType
import qualified Network.HTTP.Media.Language.Tests  as Language


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ testGroup "MediaType" MediaType.tests
    , testGroup "Accept"    Accept.tests
    , testGroup "Media"     Media.tests
    , testGroup "Language"  Language.tests
    ]
