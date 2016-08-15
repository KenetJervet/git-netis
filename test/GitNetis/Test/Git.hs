module GitNetis.Test.Git where

import           Data.Either
import           GitNetis.Git
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Git tests"
  [ testConfigCommands
  ]

testConfigCommands :: TestTree
testConfigCommands = testGroup "Git tests on config commands"
  [ testCase "Test setting config" $ do
      res <- run GitEnv (SetConfigItem testKey "bar")
      assert $ isSuccess res
  , testCase "Test getting config" $ do
      (Right output) <- run GitEnv (GetConfigItem testKey)
      "bar" @=? output
  , testCase "Test unset config" $ do
      run GitEnv (UnsetConfigItem testKey)
      res <- run GitEnv (GetConfigItem testKey)
      assert $ not (isSuccess res)
  ]
  where testKey = "git-netis.foo"
