module GitNetis.Test.Git where

import           Data.Either
import           GitNetis.Git
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Git tests"
  [ testConfigCommands
  ]

createDummyLine :: IO (Result (ErrorType SetConfigItem) (SuccessType SetConfigItem))
createDummyLine = run GitEnv (SetConfigItem dummyKey dummyVal)
  where
    dummyKey = "git-netis.dummy"
    dummyVal = "dummy"

testConfigCommands :: TestTree
testConfigCommands = testGroup "Git tests on config commands"
  [ testCase "Test getting/setting/unset config" $ do
      createDummyLine
      res <- run GitEnv (SetConfigItem testKey "bar")
      assert $ isSuccess res
      (Right output) <- run GitEnv (GetConfigItem testKey)
      "bar" @=? output
      run GitEnv (UnsetConfigItem testKey)
      res <- run GitEnv (GetConfigItem testKey)
      assert $ not (isSuccess res)
  ]
  where testKey = "git-netis.foo"
