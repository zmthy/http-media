------------------------------------------------------------------------------
module Tests (tests) where

------------------------------------------------------------------------------
import Distribution.TestSuite

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.Tests           as Media
import qualified Network.HTTP.Media.Match.Tests     as Match
import qualified Network.HTTP.Media.MediaType.Tests as MediaType


------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testGroup "MediaType" MediaType.tests
    , testGroup "Match"     Match.tests
    , testGroup "Media"     Media.tests
    ]

