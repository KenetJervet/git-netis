{-# LANGUAGE TypeFamilies #-}

module GitNetis.Git where

import           Control.Lens
import           Data.Either
import           System.Exit
import           System.IO
import           System.Process

gitExecutableName :: String
gitExecutableName = "git"

exec :: [String]  -- ^ arguments
     -> String  -- ^ input (if any)
     -> IO (ExitCode, String, String)  -- ^ (exit code, stdout, stderr)
exec args = readCreateProcessWithExitCode (proc gitExecutableName args)

exec_ = flip exec mempty

data Error = IDontCare

data GitEnv = GitEnv

type Result a = Either Error a

isSuccess :: Result a -> Bool
isSuccess = isRight

class Command cmd where
  type SuccessType cmd :: *
  type SuccessType cmd = ()
  type ErrorType cmd :: *
  type ErrorType cmd = Error
  run :: GitEnv -> cmd -> IO (Result (SuccessType cmd))


data GetConfigItem = GetConfigItem String
data SetConfigItem = SetConfigItem String String
data UnsetConfigItem = UnsetConfigItem String

instance Command GetConfigItem where
  type SuccessType GetConfigItem = String
  run _ (GetConfigItem key) = do
    (exitCode, stdout, _) <- exec_ ["config", key]
    case exitCode of
      ExitSuccess   -> return $ Right (init stdout)  -- Remove trailing \n
      ExitFailure _ -> return $ Left IDontCare

instance Command SetConfigItem where
  run _ (SetConfigItem key value) = do
    (exitCode, _, _) <- exec_ ["config", key, value]
    case exitCode of
      ExitSuccess   -> return $ Right ()
      ExitFailure _ -> return $ Left IDontCare

instance Command UnsetConfigItem where
  run _ (UnsetConfigItem key) = do
    (exitCode, _, _) <- exec_ ["config", "--unset", key]
    case exitCode of
      ExitSuccess   -> return $ Right ()
      ExitFailure _ -> return $ Left IDontCare
