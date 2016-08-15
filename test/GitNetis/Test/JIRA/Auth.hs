{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.JIRA.Auth where

import           Data.ByteString        as BS
import           Data.Either
import           GitNetis.JIRA.Auth
import           GitNetis.JIRA.Resource
import           Test.Tasty
import           Test.Tasty.HUnit

netisJIRAUrl :: String
netisJIRAUrl = "http://jira.dev.netis.com.cn:8080/rest/api/2/"

tests :: TestTree
tests = testGroup "JIRA auth tests"
  [ testCase "Incorrect credentials should fail" $ do
      let auth = BasicAuth { username = "foo", password="bar" }
          ro = RequestOptions { authOptions = auth }
      result <- getValue ro ProjectList netisJIRAUrl
      result @=? Left AuthFailed
  , testCase "Test getting all projects" $ do
      let auth = BasicAuth { username = "kenneth.zhao", password = "K3N1mx1jh2" }
          ro = RequestOptions { authOptions = auth }
      result <- getValue ro ProjectList netisJIRAUrl
      isRight result @? "isRight"
  ]
