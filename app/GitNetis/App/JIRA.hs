{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

module GitNetis.App.JIRA where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Data.ByteString.Lazy    (ByteString)
import           Data.Char
import           Data.List.Split
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
  unless (key `elem` map projectKey (projects res)) $
    throwM $ InvalidJIRAProject [i|Project does not exist: #{key}|]

setActiveJIRAProject :: String  -- ^ project key
                     -> IO ()
setActiveJIRAProject key = do
  ensureJIRAProjectExists key
  run GitEnv (SetConfigItem ActiveJIRAProject key)

getIssue :: String  -- ^ issue key
         -> IO Issue
getIssue key =
  jiraRequestJSON GetIssue{ getIssueKey = key } `catch` handler
  where
    handler NotFound =
      throwM $ InvalidJIRAIssue [i|Issue does not exist: #{key}|]

ensureIssueExists :: String  -- ^ issue key
                  -> IO ()
ensureIssueExists = void . getIssue

printProjects :: [Project] -> IO ()
printProjects projects = do
  activeProject <- getWithDefault ActiveJIRAProject ""
  putStrLn $
    renderTableWithHighlightedItem projects renderProject $
    (== activeProject) . projectKey
  where
    renderProject Project{..} = [projectKey, projectName]

printIssue :: Issue -> IO ()
printIssue Issue{..} =
  putStrLn $ renderTable [ ["Key:", issueKey]
                         , ["Type:", renderIssueType issueType]
                         , ["Status:", issueStatus]
                         , ["Assignee:", fromMaybe "-- Not assigned --" issueAssignee]
                         , ["Summary:", issueSummary]
                         ]

printIssues :: [Issue] -> IO ()
printIssues issues = do
  workingOnIssue <- getWithDefault WorkingOnIssue ""
  putStrLn $
    renderTableWithHighlightedItem issues renderIssue $
    (== workingOnIssue) . issueKey
  where
    renderIssue Issue{..} = [ issueKey
                            , renderIssueType issueType
                            , issueStatus
                            , fromMaybe "" issueAssignee
                            , issueSummary
                            ]

branchNameForIssue :: Issue
                   -> String
branchNameForIssue Issue{..} = case issueType of
  Story -> [i|feature/#{ik}|]
  Task  -> [i|task/#{ik}|]
  Bug   -> [i|bugfix/#{ik}|]
  where
    ik = map toLower issueKey

issueKeyForBranchName :: String -> String
issueKeyForBranchName = last . splitOn "/"

workonIssue :: String  -- ^ issue key
             -> IO ()
workonIssue key = withReturn $ \ret -> do
  issue <- lift $ getIssue key
  workingOnIssue <- lift $ getMaybe WorkingOnIssue
  when (isJust workingOnIssue) $ do
    let issueKey = fromJust workingOnIssue
    when (issueKey == key) $ do
      lift $ inform [i|You are already working on #{key}.|]
      ret ()
  lift $ void (jiraRequest WorkonIssue { workonIssueKey = key })
    `catchAll` onTransitionFailed
  currentBranch <- lift $ run GitEnv CurrentBranch
  let branchName = branchNameForIssue issue
  answer <- lift $ promptYesNo [i|Creating a new branch #{branchName} from #{currentBranch}. Proceed?|] True
  unless answer $ ret ()
  lift $ do
    run GitEnv CreateBranch{ name = branchNameForIssue issue
                           , startFrom = Nothing
                           , createRemote = True
                           }
    run GitEnv (SetConfigItem WorkingOnIssue key)
    inform [i|You are now working on #{key}.|]
  where
    onTransitionFailed _ =
      inform "State transition failed. It could be that the issue is already in development. In this case you can safely ignore this message."

markDone :: IO ()
markDone = withReturn $ \ret -> do
  workingOnIssue <- lift $ getMaybe WorkingOnIssue
  unless (isJust workingOnIssue) $ do
    lift $ inform "You are not working on any issue."
    ret ()
  lift $ run GitEnv (UnsetConfigItem WorkingOnIssue)
