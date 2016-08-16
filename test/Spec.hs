import           Data.IORef
import qualified GitNetis.Test.Git                as GitTest (tests)
import qualified GitNetis.Test.Resource.Bitbucket as BitbucketTest (tests)
import qualified GitNetis.Test.Resource.JIRA      as JIRATest (tests)
import           GitNetis.Test.TestConfig
import           Test.Tasty

tests :: TestTree
tests = testGroup "All tests"
  [ GitTest.tests
  , JIRATest.tests
  , BitbucketTest.tests
  ]

main :: IO ()
main = do
  config <- readConfig "test/TestConfig.json"
  writeIORef globalConfig config
  defaultMain tests
