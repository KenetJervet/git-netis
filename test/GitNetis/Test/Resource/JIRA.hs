{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.Resource.JIRA where

import           Data.ByteString        as BS
import           Data.Either
import           GitNetis.Resource
import           GitNetis.Resource.Auth
import           GitNetis.Resource.JIRA
import           Test.Tasty
import           Test.Tasty.HUnit

netisJIRARoot :: String
netisJIRARoot = "http://jira.dev.netis.com.cn:8080/rest/api/2/"

authFailedRequestOptions :: RequestOptions
authFailedRequestOptions = let
  auth = BasicAuth { username = "foo", password = "bar" }
  in
  RequestOptions { authOptions = auth, resourceRoot = netisJIRARoot }

authOKRequestOptions :: RequestOptions
authOKRequestOptions = let
  auth = BasicAuth { username = "kenneth.zhao", password = "K3N1mx1jh2"}
  in
  RequestOptions { authOptions = auth, resourceRoot = netisJIRARoot }


tests :: TestTree
tests = testGroup "JIRA tests"
  [ testAuth
  , testProjectList
  ]

testAuth :: TestTree
testAuth = testGroup "JIRA auth tests"
  [ testCase "Incorrect credentials should fail" $ do
      result <- getValue authFailedRequestOptions ProjectList
      result @=? Left AuthFailed
  ]

testProjectList :: TestTree
testProjectList = testGroup "JIRA project list"
  [ testCase "Test getting all projects" $ do
      result <- getValue authOKRequestOptions ProjectList
      isRight result @? "isRight"
  ]
