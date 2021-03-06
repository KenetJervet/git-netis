module GitNetis.Git where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Data.Either
import           Data.Maybe
import           Data.String.Interpolate
import qualified Data.Text               as T
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

data Error = GitConfigError GitConfigError
           | CreateBranchError CreateBranchError
           | IDontCare
           deriving Show

instance Exception Error


data GitConfigError = GitConfigUnexpectedError String
                    deriving Show

instance Exception GitConfigError


data CreateBranchError = BranchAlreadyExists String
                       | CreateBranchUnexpectedError String
                       deriving Show

instance Exception CreateBranchError

data GitEnv = GitEnv

class Command cmd where
  type DataType cmd :: *
  type DataType cmd = ()
  run :: GitEnv -> cmd -> IO (DataType cmd)


----------
-- Configs
----------

gitConfigPrefix :: String
gitConfigPrefix = "git-netis."

class ConfigItem configItem where
  type ValueType configItem :: *
  type ValueType configItem = String
  key :: configItem -> String
  coerceFrom :: configItem -> String -> ValueType configItem
  default coerceFrom :: (ValueType configItem ~ String) => configItem -> String -> ValueType configItem
  coerceFrom _ = id
  coerceTo :: configItem -> ValueType configItem -> String
  default coerceTo :: (ValueType configItem ~ String) => configItem -> ValueType configItem -> String
  coerceTo _ = id

data UserName = UserName
data Password = Password
data BitbucketRoot = BitbucketRoot
data JIRARoot = JIRARoot
data ActiveJIRAProject = ActiveJIRAProject
data ActiveBitbucketProject = ActiveBitbucketProject
data WorkingOnIssue = WorkingOnIssue

instance ConfigItem UserName where
  key _ = "username"

instance ConfigItem Password where
  key _ = "password"

instance ConfigItem BitbucketRoot where
  key _ = "bitbucketRoot"

instance ConfigItem JIRARoot where
  key _ = "jiraRoot"

instance ConfigItem ActiveJIRAProject where
  key _ = "activeJIRAProject"

instance ConfigItem ActiveBitbucketProject where
  key _ = "activeBitbucketProject"

instance ConfigItem WorkingOnIssue where
  key _ = "workingOnIssue"


data GetConfigItem configItem where
  GetConfigItem :: ConfigItem configItem
                => configItem
                -> GetConfigItem configItem

data SetConfigItem configItem where
  SetConfigItem :: ConfigItem configItem
                => configItem
                -> ValueType configItem
                -> SetConfigItem configItem

data UnsetConfigItem configItem where
  UnsetConfigItem :: ConfigItem configItem
                  => configItem
                  -> UnsetConfigItem configItem

instance Command (GetConfigItem item) where
  type DataType (GetConfigItem item) = ValueType item
  run _ (GetConfigItem item) = do
    (exitCode, stdout, _) <- exec_ ["config", gitConfigPrefix ++ key item]
    case exitCode of
      ExitSuccess   -> return $ coerceFrom item (init stdout)  -- Remove trailing \n
      ExitFailure _ -> throwM $ GitConfigUnexpectedError [i|Dunno what happened. Maybe key #{key item} was not found?|]

instance Command (SetConfigItem configItem) where
  run _ (SetConfigItem item value) = do
    (exitCode, _, _) <- exec_ ["config", gitConfigPrefix ++ key item, coerceTo item value]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigUnexpectedError "Really dunno what happened."

instance Command (UnsetConfigItem configItem) where
  run _ (UnsetConfigItem item) = do
    (exitCode, _, _) <- exec_ ["config", "--unset", gitConfigPrefix ++ key item]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigUnexpectedError "Really dunno what happened."

getMaybe :: ConfigItem item => item -> IO (Maybe (ValueType item))
getMaybe item = do
  val <- run GitEnv (GetConfigItem item)
  return $ Just val
  `catchAll` \_ -> return Nothing

getWithDefault :: ConfigItem item => item -> ValueType item -> IO (ValueType item)
getWithDefault item def = fromMaybe def <$> getMaybe item


------------
-- Branching
------------

data SwitchBranch = SwitchBranch String

instance Command SwitchBranch where
  run _ (SwitchBranch branchName) = do
    (exitCode, _, _) <- exec_ ["checkout", branchName]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigUnexpectedError "Really dunno what happened."


data CreateBranch = CreateBranch { name         :: String
                                 , startFrom    :: Maybe String
                                 , createRemote :: Bool
                                 }

instance Command CreateBranch where
  run _ CreateBranch{..} = do
    (exitCode, _, _) <- exec_ $ join [ ["checkout", "-b", name],
                                       catMaybes [startFrom]
                                     ]
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure 128 -> throwM $ BranchAlreadyExists name
      ExitFailure _ -> throwM $ CreateBranchUnexpectedError "Really dunno what happened."
    when createRemote $ do
      (exitCode, _, _) <- exec_ ["push", "--set-upstream", "origin", name]
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure _ -> throwM $ CreateBranchUnexpectedError "Really dunno what happened."


data CurrentBranch = CurrentBranch

instance Command CurrentBranch where
  type DataType CurrentBranch = String
  run _ CurrentBranch = do
    (exitCode, stdout, _) <- exec_ ["rev-parse", "--abbrev-ref", "HEAD"]
    case exitCode of
      ExitSuccess   -> return (T.unpack $ T.strip $ T.pack stdout)
      ExitFailure _ -> throwM IDontCare
