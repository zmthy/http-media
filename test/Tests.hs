------------------------------------------------------------------------------
module Main (main) where

------------------------------------------------------------------------------
import qualified Network.HTTP.Accept.Tests as Accept
import qualified Network.HTTP.Accept.Match.Tests as Match
import qualified Network.HTTP.Accept.MediaType.Tests as MediaType

import           Test.Framework (Test, defaultMain, testGroup)


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testGroup "Accept"    Accept.tests
    , testGroup "Match"     Match.tests
    , testGroup "MediaType" MediaType.tests
    ]

