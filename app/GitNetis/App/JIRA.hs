{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

module GitNetis.App.JIRA where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Data.ByteString.Lazy         (ByteString)
import           Data.Maybe
import           Data.String.Interpolate
import           GitNetis.App.Env
import           GitNetis.App.Util
import           GitNetis.Git
import           GitNetis.Resource
import           GitNetis.Resource.JIRA

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
    throwM $ InvalidJIRAProject [i|Project does not exist: #{key}|]

setActiveJIRAProject :: String  -- ^ project key
                     -> IO ()
setActiveJIRAProject key = do
  ensureJIRAProjectExists key
  run GitEnv (SetConfigItem ActiveJIRAProject key)

ensureIssueExists :: String  -- ^ issue key
                  -> IO ()
ensureIssueExists key =
  void $ jiraRequestJSON GetIssue{ getIssueKey = key } `catch` handler
  where
    handler NotFound =
      throwM $ InvalidJIRAIssue [i|Issue does not exist: #{key}|]

printProjects :: [Project] -> IO ()
printProjects projects = do
  activeProject <- getWithDefault "" ActiveJIRAProject
  putStrLn $
    renderTableWithHighlightedItem projects renderProject $
    (== activeProject) . projectKey
  where
    renderProject Project{..} = [projectKey, projectName]

printIssues :: [Issue] -> IO ()
printIssues issues = do
  workingOnIssue <- getWithDefault "" WorkingOnIssue
  putStrLn $
    renderTableWithHighlightedItem issues renderIssue $
    (== workingOnIssue) . issueKey
  where
    renderIssue Issue{..} = [ issueKey
                            , issueStatus
                            , fromMaybe "" issueAssignee
                            , issueSummary
                            ]

workonIssue :: String  -- ^ issue key
            -> IO ()
workonIssue key = do
  ensureIssueExists key
  void (jiraRequest WorkonIssue { workonIssueKey = key }) `catchAll` onTransitionFailed
  run GitEnv (SetConfigItem WorkingOnIssue key)
  inform [i|You are now working on #{key}.|]
  where
    onTransitionFailed _ =
      inform "State transition failed. It could be that the issue is already in development. In this case You can safely ignore this message."
