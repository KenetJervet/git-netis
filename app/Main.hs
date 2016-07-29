{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Text as T
import           Options.Applicative

data Command = Project { projectName :: Text
                       , projectCommand :: ProjectCommand
                       } deriving Show

data ProjectCommand = MyIssues
                    deriving Show

argParser :: Parser Command
argParser = Project <$>
  (T.pack <$> argument str (metavar "CODE" <> help "Project code"))
  <*>
  (const MyIssues <$> argument str (metavar "COMMAND" <> help "Command"))

exec :: Command -> IO ()
exec (Project {..}) = case projectCommand of
  MyIssues -> print "MyIssues"

main :: IO ()
main = execParser parser >>= exec
  where
    parser = info (helper <*> argParser)
      ( fullDesc
        <> progDesc "Print Manage issues, branches and everything"
      )
