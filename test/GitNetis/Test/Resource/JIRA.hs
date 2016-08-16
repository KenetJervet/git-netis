{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.Resource.JIRA where

import           Data.ByteString          as BS
import           Data.Either
import           Data.IORef
import           Data.Text                as T
import           GitNetis.Resource
import           GitNetis.Resource.Auth
import           GitNetis.Resource.JIRA
import qualified GitNetis.Test.TestConfig as Conf
import           System.IO.Unsafe
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
  globalConfig = unsafePerformIO (readIORef Conf.globalConfig)
  auth = BasicAuth { username = Conf.username . Conf.mycred $ globalConfig
                   , password = Conf.password . Conf.mycred $ globalConfig
                   }
  in
  RequestOptions { authOptions = auth, resourceRoot = netisJIRARoot }


tests :: TestTree
tests = testGroup "JIRA tests"
  [ testAuth
  , testGetProjectList
  ]

testAuth :: TestTree
testAuth = testGroup "JIRA auth tests"
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
