{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module GitNetis.Resource.JIRA where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict as H
import           Data.IORef
import           Data.List
import           Data.Maybe
import qualified Data.Vector         as V
import           GHC.Generics
import           GitNetis.Git
import           GitNetis.Resource
import           GitNetis.Util
import           Text.Printf

class Resource res => JIRAResource res

--------------
-- GetProjectList
--------------

data Project = Project { projectKey  :: String
                       , projectName :: String
                       } deriving Show

newtype ProjectList = ProjectList { projects :: [Project]
                                  } deriving Show

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    name <- obj .: "name"
    return Project{ projectKey = key, projectName = name }

instance FromJSON ProjectList where
  parseJSON = withArray "project objects" $ \obj ->
    ProjectList <$> (mapM parseJSON . V.toList) obj


data GetProjectList = GetProjectList deriving JIRAResource

instance Resource GetProjectList where
  uri _ = "project"

instance JSONResource ProjectList GetProjectList


---------------
-- GetIssueList
---------------

data Issue = Issue { issueKey      :: String
                   , issueSummary  :: String
                   , issueAssignee :: Maybe String
                   , issueStatus   :: String
                   } deriving Show

data IssueList = IssueList { issues :: [Issue]
                           } deriving Show

instance FromJSON Issue where
  parseJSON = withObject "issue object" $ \obj -> do
    key <- obj .: "key"
    fields <- obj .: "fields"
    summary <- fields .: "summary"
    assignee <- fields .:? "assignee"
    assigneeName <- (if isNothing assignee then H.empty else fromJust assignee) .:? "name"
    status <- (fields .: "status") >>= (.: "name")
    return Issue{ issueKey = key
                , issueSummary = summary
                , issueAssignee = assigneeName
                , issueStatus = status
                }

instance FromJSON IssueList where
  parseJSON = withObject "issue list" $ \obj -> do
    issues <- obj .: "issues"
    return IssueList{ issues = issues }

data GetIssueList = GetIssueList { getIssueListAll      :: Bool
                                 , getIssueListFreeOnly :: Bool
                                 , getIssueListToDoOnly :: Bool
                                 }
deriving instance JIRAResource GetIssueList

instance Resource GetIssueList where
  uriIO GetIssueList{..} = do
    -- TODO: Better url joining
    (paramsIORef :: IORef [String]) <- newIORef ["sprint+in+openSprints()"]
    currentProject <- run GitEnv (GetConfigItem "activeJIRAProject")
    modifyIORef' paramsIORef (printf "project=%s" currentProject:)
    if getIssueListFreeOnly
      then
      modifyIORef' paramsIORef ("assignee+is+empty":)
      else
      when (not getIssueListAll) $ do
      currentUser <- run GitEnv (GetConfigItem "username")
      modifyIORef' paramsIORef (printf "assignee=%s" currentUser:)
    when getIssueListToDoOnly $ do
      modifyIORef' paramsIORef ("status=open":)
    params <- readIORef paramsIORef
    return $ printf "search?jql=%s" $ intercalate "+and+" params

instance JSONResource IssueList GetIssueList


data GetIssue = GetIssue { getIssueKey :: String
                         }
deriving instance JIRAResource GetIssue

instance Resource GetIssue where
  uri GetIssue{..} = printf "issue/%s" getIssueKey

instance JSONResource Issue GetIssue
