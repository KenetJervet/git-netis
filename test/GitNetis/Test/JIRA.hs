module GitNetis.Test.JIRA where

import           GitNetis.Test.JIRA.Auth as Auth (tests)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "JIRA tests"
  [ Auth.tests
  ]
