{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module GitNetis.Resource.JIRA where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.QQ
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.List
import           Data.Maybe
import qualified Data.Vector         as V
import           GHC.Generics
import           GitNetis.Git
import           GitNetis.Resource
import           GitNetis.Util
import           Text.Printf

class Resource method res => JIRAResource method res

--------
-- Utils
--------

data JQLQuery = JQLEq String String
              | JQLIsEmpty String
              | JQLIn String String

jqlQueryToString :: JQLQuery -> String
jqlQueryToString q = case q of
  JQLEq k v -> printf "%s=%s" k v
  JQLIsEmpty k -> printf "%s+is+empty" k
  JQLIn k v -> printf "%s+in+%s" k v

assembleJQL :: [Maybe JQLQuery] -> String
assembleJQL queries =
  let cleanedQueries = catMaybes queries
  in
    intercalate "+and+" (map jqlQueryToString cleanedQueries)
  where
    assembleKVP (k, v) =
      case v of
        Nothing -> printf "%s+is+empty" k
        (Just jv) -> printf "%s=%s" k jv

-------------
-- Data decls
-------------

data Project = Project { projectKey  :: String
                       , projectName :: String
                       } deriving Show

newtype ProjectList = ProjectList { projects :: [Project]
                                  } deriving Show

data Issue = Issue { issueKey      :: String
                   , issueSummary  :: String
                   , issueAssignee :: Maybe String
                   , issueStatus   :: String
                   } deriving Show

data IssueList = IssueList { issues :: [Issue]
                           } deriving Show


------------
-- Resources
------------

data GetProjectList = GetProjectList

data GetIssueList = GetIssueList { getIssueListActiveProject   :: String
                                 , getIssueListAssignee        :: Maybe String
                                 , getIssueListStatus          :: Maybe String
                                 , getIssueListOnlyOpenSprints :: Bool
                                 }

data GetIssue = GetIssue { getIssueKey :: String
                         }

data WorkonIssue = WorkonIssue { workonIssueKey :: String
                               }


---------------------
-- Resource instances
---------------------

instance Resource HttpGet GetIssueList where
  uri GetIssueList{..} = do
    -- TODO: Better url joining
    let activeProjectQuery = Just $ JQLEq "project" getIssueListActiveProject
        assigneeQuery = maybe Nothing
          (
            \assignee -> if assignee == ""
                         then Just $ JQLIsEmpty "assignee"
                         else Just $ JQLEq "assignee" assignee
          ) getIssueListAssignee
        sprintQuery = if getIssueListOnlyOpenSprints
                      then Just $ JQLIn "sprint" "openSprints()"
                      else Nothing
        statusQuery = JQLEq "status" <$> getIssueListStatus
    return $ printf "search?jql=%s" $ assembleJQL
      [ activeProjectQuery
      , assigneeQuery
      , sprintQuery
      , statusQuery
      ]


instance Resource HttpGet GetIssue where
  uri GetIssue{..} = return $ printf "issue/%s" getIssueKey


instance Resource HttpGet GetProjectList where
  uri _ = return "project"


instance Resource HttpPost WorkonIssue where
  uri WorkonIssue{..} = return $ printf "issue/%s/transitions" workonIssueKey
  -- payload WorkonIssue{..} = return $ HttpPostJSONPayload $ H.fromList [("SDF", "SDF")] :: [(String, String)]
  payload WorkonIssue{..} = return $ HttpPostJSONPayload $
    [aesonQQ|{
               update: {
                 comment: [{
                   add: {
                     body: "--- Transition done from git-netis tool. Consult kenneth.zhao for further information"
                   }
                 }]
               },
               transition: {
                 id: "4"
               }
             }|]


-----------------
-- JSON instances
-----------------

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    name <- obj .: "name"
    return Project{ projectKey = key, projectName = name }

instance FromJSON ProjectList where
  parseJSON = withArray "project objects" $ \obj ->
    ProjectList <$> (mapM parseJSON . V.toList) obj

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


--------------------------
-- JSON resource instances
--------------------------

instance JSONResource ProjectList HttpGet GetProjectList

instance JSONResource IssueList HttpGet GetIssueList

instance JSONResource Issue HttpGet GetIssue


--------------------------
-- JIRA resource instances
--------------------------

instance JIRAResource HttpGet GetProjectList

instance JIRAResource HttpGet GetIssueList

instance JIRAResource HttpGet GetIssue

instance JIRAResource HttpPost WorkonIssue
