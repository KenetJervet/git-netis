import qualified GitNetis.Test.Resource.JIRA as JIRATest (tests)
import qualified GitNetis.Test.Resource.Bitbucket as BitbucketTest (tests)
import           Test.Tasty

tests :: TestTree
tests = testGroup "All tests"
  [ JIRATest.tests
  , BitbucketTest.tests
  ]

main :: IO ()
main = defaultMain tests
