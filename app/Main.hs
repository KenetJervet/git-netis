{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Text                   as T
import           GitNetis.App
import           GitNetis.Resource.Bitbucket as RB
import GitNetis.Resource.JIRA as RJ
import           Options.Applicative
import           Text.Printf

data Command = BitbucketCommand BitbucketCommand
             | JIRACommand JIRACommand

data BitbucketCommand = BitbucketListProjects
data JIRACommand = JIRAListProjects

argParser :: Parser Command
argParser = subparser
  ( command "bitbucket" (info (BitbucketCommand <$> bitbucketParser) idm)
    <>
    command "jira" (info (JIRACommand <$> jiraParser) idm)
  )

bitbucketParser :: Parser BitbucketCommand
bitbucketParser = pure BitbucketListProjects

jiraParser :: Parser JIRACommand
jiraParser = pure JIRAListProjects

exec :: Command -> IO ()
exec (BitbucketCommand cmd) = execBitbucketCommand cmd
exec (JIRACommand cmd)      = execJIRACommand cmd

execBitbucketCommand :: BitbucketCommand -> IO ()
execBitbucketCommand BitbucketListProjects = do
  res <- bitbucketRequest RB.GetProjectList
  renderWithSeqNum (RB.projects res) renderProject
  where
    renderProject :: RB.Project -> String
    renderProject RB.Project{..} =
      printf "%s\t%s" projectKey projectDescription


execJIRACommand :: JIRACommand -> IO ()
execJIRACommand JIRAListProjects = do
  res <- jiraRequest RJ.GetProjectList
  renderWithSeqNum (RJ.projects res) renderProject
  where
    renderProject :: RJ.Project -> String
    renderProject RJ.Project{..} =
      printf "%s\t%s" projectKey projectName

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
