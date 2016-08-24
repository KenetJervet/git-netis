{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeFamilies        #-}

module GitNetis.Git where

import           Control.Lens
import           Control.Monad.Catch
import           Data.Either
import           Data.String.Interpolate
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

data Error = GitConfigError String
           | IDontCare
           deriving Show

instance Exception Error

data GitEnv = GitEnv

class Command cmd where
  type DataType cmd :: *
  type DataType cmd = ()
  run :: GitEnv -> cmd -> IO (DataType cmd)

gitConfigPrefix :: String
gitConfigPrefix = "git-netis."

class ConfigItem configItem where
  type ValueType configItem :: *
  type ValueType configItem = String
  key :: configItem -> String
  coerceFrom :: configItem -> String -> ValueType configItem
  default coerceFrom :: configItem -> String -> String
  coerceFrom _ = id
  coerceTo :: configItem -> ValueType configItem -> String
  default coerceTo :: configItem -> String -> String
  coerceTo _ = id

data UserName = UserName
data Password = Password
data BitbucketRoot = BitbucketRoot
data JIRARoot = JIRARoot
data ActiveJIRAProject = ActiveJIRAProject
data ActiveBitbucketProject = ActiveBitbucketProject

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
      ExitFailure _ -> throwM $ GitConfigError [i|Dunno what happened. Maybe key #{key item} was not found?|]

instance Command (SetConfigItem configItem) where
  run _ (SetConfigItem item value) = do
    (exitCode, _, _) <- exec_ ["config", gitConfigPrefix ++ key item, coerceTo item value]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigError "Really dunno what happened."

instance Command (UnsetConfigItem configItem) where
  run _ (UnsetConfigItem item) = do
    (exitCode, _, _) <- exec_ ["config", "--unset", gitConfigPrefix ++ key item]
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwM $ GitConfigError "Really dunno what happened."
