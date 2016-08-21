module GitNetis.App.Env where

import           Data.IORef
import           GitNetis.Git
import           System.IO.Unsafe

data Env = Env { username      :: String
               , password      :: String
               , jiraRoot      :: String
               , bitbucketRoot :: String
               }

{-# NOINLINE globalEnv #-}
globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef Env{}

setGlobalEnv :: IO Env
setGlobalEnv = do
  username <- run GitEnv (GetConfigItem UserName)
  password <- run GitEnv (GetConfigItem Password)
  bitbucketRoot <- run GitEnv (GetConfigItem BitbucketRoot)
  jiraRoot <- run GitEnv (GetConfigItem JIRARoot)
  let env = Env { username = username
                , password = password
                , jiraRoot = jiraRoot
                , bitbucketRoot = bitbucketRoot
                }
  writeIORef globalEnv env
  return env
