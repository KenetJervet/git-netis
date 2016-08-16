{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.Resource.Bitbucket where

import           Data.Either
import           GitNetis.Resource
import           GitNetis.Resource.Auth
import           GitNetis.Resource.Bitbucket
import           Test.Tasty
import           Test.Tasty.HUnit

netisBitbucketRoot :: String
netisBitbucketRoot = "https://git.dev.netis.com/rest/api/1.0/"

authFailedRequestOptions :: RequestOptions
authFailedRequestOptions = let
  auth = BasicAuth { username = "foo", password = "bar" }
  in
  RequestOptions { authOptions = auth, resourceRoot = netisBitbucketRoot }

authOKRequestOptions :: RequestOptions
authOKRequestOptions = let
  auth = BasicAuth { username = "kenneth.zhao", password = "K3N1mx1jh2"}
  in
  RequestOptions { authOptions = auth, resourceRoot = netisBitbucketRoot }


tests :: TestTree
tests = testGroup "Bitbucket tests"
  [ testAuth
  , testGetProjectList
  ]

testAuth :: TestTree
testAuth = testGroup "Bitbucket auth tests"
  [ testCase "Incorrect credentials should fail" $ do
      result <- getValue authFailedRequestOptions GetProjectList
      result @=? Left AuthFailed
  ]

testGetProjectList :: TestTree
testGetProjectList = testGroup "JIRA project list"
  [ testCase "Test getting all projects" $ do
      result <- getValue authOKRequestOptions GetProjectList
      isRight result @? "isRight"
  ]
