{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.Resource.JIRA where

import           Control.Monad.Catch
import           Data.ByteString          as BS
import           Data.Either
import           Data.IORef
import           Data.Maybe
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
  auth = BasicAuth { username = Conf.username . Conf.myCred $ globalConfig
                   , password = Conf.password . Conf.myCred $ globalConfig
                   }
  in
  RequestOptions { authOptions = auth, resourceRoot = netisJIRARoot }


tests :: TestTree
tests = testGroup "JIRA tests"
  [ testAuth
  , testGetProjectList
  , testGetIssueList
  ]

testAuth :: TestTree
testAuth = testGroup "JIRA auth tests"
  [ testCase "Incorrect credentials should fail" $ do
      result <- (Just <$> getValue authFailedRequestOptions GetProjectList) `catch` \AuthFailed -> return Nothing
      assert $ isNothing result
  ]

testGetProjectList :: TestTree
testGetProjectList = testGroup "JIRA project list"
  [ testCase "Test getting all projects" $ do
      result <- getValue authOKRequestOptions GetProjectList
      return ()
  ]

testGetIssueList :: TestTree
testGetIssueList = testGroup "JIRA issue list"
  [ testCase "Test getting issues assigned to me" $ do
      testConfig <- readIORef Conf.globalConfig
      let activeProject = Conf.activeJIRAProject . Conf.myJIRAConfig $ testConfig
      let me = Conf.username . Conf.myCred $ testConfig
      result <- getJSON authOKRequestOptions
                  GetIssueList{ getIssueListActiveProject = activeProject
                              , getIssueListAssignee = Just me
                              , getIssueListStatus = Nothing
                              , getIssueListOnlyOpenSprints = False
                              }
      mapM_ (assertEqual "assignee" (Just me)) (issueAssignee <$> issues result)
  , testCase "Test getting free issues" $ do
      testConfig <- readIORef Conf.globalConfig
      let activeProject = Conf.activeJIRAProject . Conf.myJIRAConfig $ testConfig
      let me = Conf.username . Conf.myCred $ testConfig
      result <- getJSON authOKRequestOptions
                  GetIssueList{ getIssueListActiveProject = activeProject
                              , getIssueListAssignee = Just ""
                              , getIssueListStatus = Nothing
                              , getIssueListOnlyOpenSprints = False
                              }
      mapM_ (assertEqual "assignee" Nothing) (issueAssignee <$> issues result)
  ]
