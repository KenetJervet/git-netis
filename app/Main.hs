{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Text                   as T
import           GitNetis.App
import qualified GitNetis.Git                as G
import           GitNetis.Resource.Bitbucket as RB
import           GitNetis.Resource.JIRA      as RJ
import           Options.Applicative
import           Text.Printf

-----------
-- Commands
-----------

data Command = BitbucketCommand BitbucketCommand
             | JIRACommand JIRACommand
             | IssueCommand IssueCommand

data BitbucketCommand = BitbucketListProjects
                      | BitbucketWorkonProject String

data JIRACommand = JIRAListProjects
                 | JIRAWorkonProject String

data IssueCommand = IssueListIssues { issueListAll :: Bool
                              }
                  | IssueWorkon String


----------
-- Parsers
----------

argParser :: Parser Command
argParser = subparser
  ( command "bitbucket" (info (helper <*> (BitbucketCommand <$> bitbucketParser)) idm)
    <>
    command "jira" (info (helper <*> (JIRACommand <$> jiraParser)) idm)
    <>
    command "issue" (info (helper <*> (IssueCommand <$> issueParser)) idm)
  )

-----------------------
-- Bitbucket subparsers
-----------------------

bitbucketParser :: Parser BitbucketCommand
bitbucketParser = subparser
  ( command "list-projects" ( info
                              (helper <*> pure BitbucketListProjects)
                              (progDesc "List all visible projects")
                            )
    <>
    command "workon-project" ( info
                               ( helper
                                 <*>
                                 (BitbucketWorkonProject <$> bitbucketWorkonProjectParser)
                               )
                               (progDesc "Workon a selected project")
                             )
  )

bitbucketWorkonProjectParser :: Parser String
bitbucketWorkonProjectParser = strArgument
                               ( metavar "key"
                               <>
                                 help "The key of the project"
                               )

------------------
-- JIRA subparsers
------------------

jiraParser :: Parser JIRACommand
jiraParser = subparser
  ( command "list-projects" ( info
                              (helper <*> pure JIRAListProjects)
                              (progDesc "List all visible projects")
                            )
    <>
    command "workon-project" ( info
                               ( helper
                                 <*>
                                 (JIRAWorkonProject <$> jiraWorkonProjectParser)
                               )
                               (progDesc "Workon a selected project")
                             )
  )

jiraWorkonProjectParser :: Parser String
jiraWorkonProjectParser = strArgument
                          ( metavar "key"
                            <>
                            help "The key of the project"
                          )

--------------------
-- Issues subparsers
--------------------

issueParser :: Parser IssueCommand
issueParser = subparser
  ( command "list" ( info
                     (helper <*> issueListParser)
                     (progDesc "List issues")
                   )
    <>
    command "workon" ( info
                       (helper <*> (IssueWorkon <$> issueWorkonParser))
                       (progDesc "Start working on an issue")
                     )
  )

issueListParser :: Parser IssueCommand
issueListParser = IssueListIssues
  <$> switch ( long "all"
               <> short 'a'
               <> help "Show all issues instead of only issues assigned to me"
             )

issueWorkonParser :: Parser String
issueWorkonParser = strArgument
                    ( metavar "key"
                      <>
                      help "The key of the issue"
                    )

-------------------------
-- Exec Bitbucket command
-------------------------

exec :: Command -> IO ()
exec (BitbucketCommand cmd) = execBitbucketCommand cmd
exec (JIRACommand cmd)      = execJIRACommand cmd
exec (IssueCommand cmd)     = execIssueCommand cmd

execBitbucketCommand :: BitbucketCommand -> IO ()
execBitbucketCommand cmd = case cmd of
  BitbucketListProjects -> do
    res <- bitbucketRequest RB.GetProjectList
    renderWithSeqNum (RB.projects res) renderProject
      where
        renderProject :: RB.Project -> String
        renderProject RB.Project{..} =
          printf "%s\t%s" projectKey projectDescription
  BitbucketWorkonProject key -> setActiveBitbucketProject key


-------------------------
-- Exec JIRA command
-------------------------

execJIRACommand :: JIRACommand -> IO ()
execJIRACommand cmd = case cmd of
  JIRAListProjects -> do
    res <- jiraRequest RJ.GetProjectList
    renderWithSeqNum (RJ.projects res) renderProject
      where
        renderProject :: RJ.Project -> String
        renderProject RJ.Project{..} =
          printf "%s\t%s" projectKey projectName
  JIRAWorkonProject key -> setActiveJIRAProject key


---------------------
-- Exec Issue command
---------------------

execIssueCommand :: IssueCommand -> IO ()
execIssueCommand cmd = case cmd of
  IssueListIssues{..} -> do
    res <- jiraRequest RJ.GetIssueList{ getIssueListAll = issueListAll }
    renderWithSeqNum (RJ.issues res) renderIssue
      where
        renderIssue :: RJ.Issue -> String
        renderIssue RJ.Issue{..} =
          printf "%s\t%s\t%s" issueKey (maybe "\t" id issueAssignee) issueSummary
  IssueWorkon key -> do
    workonIssue key

main :: IO ()
main = do
  cmd <- execParser parser
  setGlobalEnv
  exec cmd
  where
    parser = info (helper <*> argParser)
      ( fullDesc
        <> progDesc "A Netis internal Git utility that integrates with JIRA and Bitbucket"
      )
