import qualified GitNetis.Test.JIRA as JIRATest (tests)
import           Test.Tasty

tests :: TestTree
tests = testGroup "All tests"
  [ JIRATest.tests
  ]

main :: IO ()
main = defaultMain tests
