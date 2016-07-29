{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.JIRA where

import           Data.ByteString      as BS
import           Data.ByteString.Lazy as BSL
import           GitNetis.JIRA        (baidu)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "JIRA tests"
  [ testCase "Wreq to Baidu" $ do
      resp <- baidu
      ("<html" `BS.isInfixOf` BSL.toStrict resp) @? "Incorrect response"
  ]
