------------------------------------------------------------------------------
module Main (main) where

------------------------------------------------------------------------------
import           Test.Framework

------------------------------------------------------------------------------
import qualified Network.HTTP.Accept.Tests as Accept
import qualified Network.HTTP.Accept.Match.Tests as Match
import qualified Network.HTTP.Accept.MediaType.Tests as MediaType


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- interpretArgsOrExit []
    defaultMainWithOpts tests args { ropt_hide_successes = Just True }


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testGroup "MediaType" MediaType.tests
    , testGroup "Match"     Match.tests
    , testGroup "Accept"    Accept.tests
    ]

