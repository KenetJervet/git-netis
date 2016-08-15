{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.JIRA.Auth where

import           Data.ByteString      as BS
import           GitNetis.JIRA.Auth
import           Test.Tasty
import           Test.Tasty.HUnit

netisJIRAUrl :: String
netisJIRAUrl = "http://jira.dev.netis.com.cn:8080/rest/api/2"

tests :: TestTree
tests = testGroup "JIRA auth tests"
  []
