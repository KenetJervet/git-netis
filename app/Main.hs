{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Text as T
import           Options.Applicative

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
exec (JIRACommand cmd) = execJIRACommand cmd

execBitbucketCommand :: BitbucketCommand -> IO ()
execBitbucketCommand = undefined


execJIRACommand :: JIRACommand -> IO ()
execJIRACommand = undefined

main :: IO ()
main = execParser parser >>= exec
  where
    parser = info (helper <*> argParser)
      ( fullDesc
        <> progDesc "A Netis internal Git utility that integrates with JIRA and Bitbucket"
      )
