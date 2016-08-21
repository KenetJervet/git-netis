{-# LANGUAGE MultiParamTypeClasses #-}

module GitNetis.App.JIRA where

import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString.Lazy   (ByteString)
import           GitNetis.App.Env
import           GitNetis.App.Util
import           GitNetis.Git
import           GitNetis.Resource
import           GitNetis.Resource.JIRA
import           Text.Printf

data JIRAError = InvalidJIRAProject String
               | InvalidJIRAIssue String
               deriving Show

instance Exception JIRAError


jiraRequest :: (JIRAResource method res, Resource method res)
                 => res
                 -> IO ByteString
jiraRequest res = request res jiraRoot

jiraRequestJSON :: (JIRAResource method res, JSONResource json method res) => res -> IO json
jiraRequestJSON res = requestJSON res jiraRoot


ensureJIRAProjectExists :: String  -- ^ project key
                        -> IO ()
ensureJIRAProjectExists key = do
  res <- jiraRequestJSON GetProjectList
  unless (key `elem` (map projectKey (projects res))) $
    throwM $ InvalidJIRAProject (printf "Project does not exist: %s" key)

setActiveJIRAProject :: String  -- ^ project key
                     -> IO ()
setActiveJIRAProject key = do
  ensureJIRAProjectExists key
  run GitEnv (SetConfigItem ActiveJIRAProject key)

ensureIssueExists :: String  -- ^ issue key
                  -> IO ()
ensureIssueExists key = do
  void $ jiraRequestJSON GetIssue{ getIssueKey = key } `catch` handler
  where
    handler NotFound = do
      throwM $ InvalidJIRAIssue (printf "Issue does not exist: %s" key)

workonIssue :: String  -- ^ issue key
            -> IO ()
workonIssue key = do
  ensureIssueExists key
  void $ jiraRequest WorkonIssue { workonIssueKey = key }
  inform $ printf "You are now working on %s.\n" key
