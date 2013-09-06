------------------------------------------------------------------------------
module Tests (tests) where

------------------------------------------------------------------------------
import qualified Network.HTTP.Accept.Tests as Accept
import qualified Network.HTTP.Accept.Match.Tests as Match
import qualified Network.HTTP.Accept.MediaType.Tests as MediaType
------------------------------------------------------------------------------
import Distribution.TestSuite


------------------------------------------------------------------------------
{-main :: IO ()-}
{-main = do-}
    {-args <- interpretArgsOrExit []-}
    {-defaultMainWithOpts tests args { ropt_hide_successes = Just True }-}


------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testGroup "MediaType" MediaType.tests
    , testGroup "Match"     Match.tests
    , testGroup "Accept"    Accept.tests
    ]

