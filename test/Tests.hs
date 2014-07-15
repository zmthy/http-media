------------------------------------------------------------------------------
module Tests (tests) where

------------------------------------------------------------------------------
import Distribution.TestSuite

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.Tests           as Media
import qualified Network.HTTP.Media.Accept.Tests    as Accept
import qualified Network.HTTP.Media.MediaType.Tests as MediaType


------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testGroup "MediaType" MediaType.tests
    , testGroup "Accept"    Accept.tests
    , testGroup "Media"     Media.tests
    ]

