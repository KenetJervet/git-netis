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

data Dummy = Dummy

instance ConfigItem Dummy where
  key _ = "dummy"

createDummyLine :: IO (DataType (SetConfigItem Dummy))
createDummyLine = run GitEnv (SetConfigItem Dummy dummyVal)
  where
    dummyKey = "dummy"
    dummyVal = "foo"

testConfigCommands :: TestTree
testConfigCommands = testGroup "Git tests on config commands"
  [ testCase "Test getting/setting/unset config" $ do
      createDummyLine
      _ <- run GitEnv (SetConfigItem Dummy "bar")
      -- Should not fail at here
      output <- run GitEnv (GetConfigItem Dummy)
      "bar" @=? output
      run GitEnv (UnsetConfigItem Dummy)
      res <- (Just <$> run GitEnv (GetConfigItem Dummy)) `catch` \(_ :: Error) -> return Nothing
      assert $ isNothing res
  ]
