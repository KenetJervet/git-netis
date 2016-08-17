{-# LANGUAGE ScopedTypeVariables #-}

module GitNetis.Test.Git where

import           Control.Monad.Catch
import           Data.Either
import           Data.Maybe
import           GitNetis.Git
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Git tests"
  [ testConfigCommands
  ]

createDummyLine :: IO (DataType SetConfigItem)
createDummyLine = run GitEnv (SetConfigItem dummyKey dummyVal)
  where
    dummyKey = "git-netis.dummy"
    dummyVal = "dummy"

testConfigCommands :: TestTree
testConfigCommands = testGroup "Git tests on config commands"
  [ testCase "Test getting/setting/unset config" $ do
      createDummyLine
      _ <- run GitEnv (SetConfigItem testKey "bar")
      -- Should not fail at here
      output <- run GitEnv (GetConfigItem testKey)
      "bar" @=? output
      run GitEnv (UnsetConfigItem testKey)
      res <- (Just <$> run GitEnv (GetConfigItem testKey)) `catch` \(_ :: Error) -> return Nothing
      assert $ isNothing res
  ]
  where testKey = "git-netis.foo"
