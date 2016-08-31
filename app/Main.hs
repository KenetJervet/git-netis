{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Catch
import           Data.Maybe
import           Data.String.Interpolate
import qualified Data.Text                   as T
import           GitNetis.App.Bitbucket      as AB
import           GitNetis.App.Env
import           GitNetis.App.JIRA           as AJ
import           GitNetis.App.Util           hiding (InputMode (..))
import           GitNetis.Git                hiding (Command, exec)
import qualified GitNetis.Git                as G
import           GitNetis.Resource.Bitbucket as RB
import           GitNetis.Resource.JIRA      as RJ
import           Options.Applicative

-----------
-- Commands
-----------

data Command = SetupCommand SetupCommand
             | BitbucketCommand BitbucketCommand
             | JIRACommand JIRACommand
             | IssueCommand IssueCommand

data SetupCommand = Setup { interactive :: Bool }

data BitbucketCommand = BitbucketListProjects
                      | BitbucketWorkonProject String

data JIRACommand = JIRAListProjects
                 | JIRAWorkonProject String

data IssueCommand = IssueListIssues { issueListAll      :: Bool
                                    , issueListFreeOnly :: Bool
                                    , issueListToDoOnly :: Bool
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
    <>
    command "setup" (info (helper <*> (SetupCommand <$> setupParser)) idm)
  )

-------------------
-- Setup subparsers
-------------------

setupParser :: Parser SetupCommand
setupParser =
  Setup <$> switch ( long "interactive"
                     <> short 'i'
                     <> help "Setup git-netis environment"
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
  <*> switch ( long "free"
               <> short 'f'
               <> help "Show only free issues (those that are not assigned to anyone) \
                       \ Note that this will override `--all` option"
             )
  <*> switch ( long "todo"
               <> short 't'
               <> help "Show only issues in the `To do` status"
             )

issueWorkonParser :: Parser String
issueWorkonParser = strArgument
                    ( metavar "key"
                      <>
                      help "The key of the issue"
                    )

-------
-- Exec
-------

exec :: Command -> IO ()
exec (SetupCommand cmd) = execSetupCommand cmd
exec cmd = do
  loadGlobalEnv
  case cmd of
    BitbucketCommand cmd -> execBitbucketCommand cmd
    JIRACommand cmd      -> execJIRACommand cmd
    IssueCommand cmd     -> execIssueCommand cmd


---------------------
-- Exec Setup command
---------------------

execSetupCommand :: SetupCommand -> IO ()
execSetupCommand cmd = case cmd of
  Setup{..} -> do
    savedUserName <- getWithDefault UserName ""
    savedPassword <- getWithDefault Password ""
    username <- validatedPromptWithDefault "Your user name:" savedUserName (return . not . null)
    password <- validatedPromptPassword "Your password:" (return . not . null)
    run GitEnv (SetConfigItem UserName username)
    run GitEnv (SetConfigItem Password password)
    inform ""
    inform "Your username and password have been saved."
    savedJiraRoot <- getWithDefault JIRARoot "http://jira.dev.netis.com.cn:8080/rest/api/2/"
    savedBitbucketRoot <- getWithDefault BitbucketRoot "https://git.dev.netis.com/rest/api/1.0/"
    jiraRoot <- promptWithDefault "JIRA root URL" savedJiraRoot
    bitbucketRoot <- promptWithDefault "Bitbucket root URL" savedBitbucketRoot
    run GitEnv (SetConfigItem JIRARoot jiraRoot)
    run GitEnv (SetConfigItem BitbucketRoot bitbucketRoot)
    setGlobalEnv Env{ username
                    , password
                    , jiraRoot
                    , bitbucketRoot
                    }
    res <- bitbucketRequestJSON RB.GetProjectList
    AB.printProjects (RB.projects res)
    project <- prompt "Select a project to work with: "
    inform [i|You are now working on #{project}.|]

-------------------------
-- Exec Bitbucket command
-------------------------

execBitbucketCommand :: BitbucketCommand -> IO ()
execBitbucketCommand cmd = case cmd of
  BitbucketListProjects      -> do
    res <- bitbucketRequestJSON RB.GetProjectList
    AB.printProjects $ RB.projects res
  BitbucketWorkonProject key -> setActiveBitbucketProject key


-------------------------
-- Exec JIRA command
-------------------------

execJIRACommand :: JIRACommand -> IO ()
execJIRACommand cmd = case cmd of
  JIRAListProjects      -> do
    res <- jiraRequestJSON RJ.GetProjectList
    AJ.printProjects $ RJ.projects res
  JIRAWorkonProject key -> setActiveJIRAProject key


---------------------
-- Exec Issue command
---------------------

execIssueCommand :: IssueCommand -> IO ()
execIssueCommand cmd = case cmd of
  IssueListIssues{..} -> do
    activeProject <- run GitEnv (GetConfigItem ActiveJIRAProject)
    currentUser <- run GitEnv (GetConfigItem UserName)
    let assignee = [ if issueListFreeOnly then "" else currentUser | not issueListAll ]
        status = [ "open" | issueListToDoOnly ]
    res <- jiraRequestJSON RJ.GetIssueList{ getIssueListActiveProject = activeProject
                                          , getIssueListAssignee = assignee
                                          , getIssueListStatus = status
                                          , getIssueListOnlyOpenSprints = True
                                          }
    AJ.printIssues $ RJ.issues res
  IssueWorkon key ->
    workonIssue key

main :: IO ()
main = do
  cmd <- execParser parser
  exec cmd `catchAll` mainErrorHandler
  where
    parser = info (helper <*> argParser)
      ( fullDesc
        <> progDesc "A Netis internal Git utility that integrates with JIRA and Bitbucket"
      )
    mainErrorHandler = print
