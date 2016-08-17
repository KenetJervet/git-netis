{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module GitNetis.Git where

import           Control.Lens
import           Control.Monad.Catch
import           Data.Either
import           System.Exit
import           System.IO
import           System.Process
import           Text.Printf

gitExecutableName :: String
gitExecutableName = "git"

exec :: [String]  -- ^ arguments
     -> String  -- ^ input (if any)
     -> IO (ExitCode, String, String)  -- ^ (exit code, stdout, stderr)
exec args = readCreateProcessWithExitCode (proc gitExecutableName args)

exec_ = flip exec mempty

data Error = GitConfigError String
           | IDon'tCare
           deriving Show

instance Exception Error

data GitEnv = GitEnv

class Command cmd where
  type DataType cmd :: *
  type DataType cmd = ()
  run :: GitEnv -> cmd -> IO (DataType cmd)


data GetConfigItem = GetConfigItem String
data SetConfigItem = SetConfigItem String String
data UnsetConfigItem = UnsetConfigItem String

instance Command GetConfigItem where
  type DataType GetConfigItem = String
  run _ (GetConfigItem key) = do
    (exitCode, stdout, _) <- exec_ ["config", key]
    case exitCode of
      ExitSuccess   -> return $ init stdout  -- Remove trailing \n
      ExitFailure _ -> throwM $ GitConfigError $ printf "Dunno what happened. Maybe key `%s` was not found?" key

instance Command SetConfigItem where
  run _ (SetConfigItem key value) = do
    (exitCode, _, _) <- exec_ ["config", key, value]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigError $ printf "Really dunno what happened."

instance Command UnsetConfigItem where
  run _ (UnsetConfigItem key) = do
    (exitCode, _, _) <- exec_ ["config", "--unset", key]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigError $ printf "Really dunno what happened."
