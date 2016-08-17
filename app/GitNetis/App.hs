{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitNetis.App where

import           Control.Monad
import           Control.Monad.Catch
import           Data.IORef
import qualified Data.Text                   as T
import           GitNetis.Git                hiding (Error)
import           GitNetis.Resource           hiding (Error)
import qualified GitNetis.Resource.Auth      as A
import           GitNetis.Resource.Bitbucket as RB
import           GitNetis.Resource.JIRA      as RJ
import           System.IO.Unsafe
import           Text.Printf

data Env = Env { username      :: String
               , password      :: String
               , jiraRoot      :: String
               , bitbucketRoot :: String
               }

data Error = InvalidBitbucketProject String
           | InvalidJIRAProject String
           | InvalidJIRAIssue String
           deriving Show

instance Exception Error

{-# NOINLINE globalEnv #-}
globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef Env{}

setGlobalEnv :: IO Env
setGlobalEnv = do
  username <- run GitEnv (GetConfigItem "username")
  password <- run GitEnv (GetConfigItem "password")
  bitbucketRoot <- run GitEnv (GetConfigItem "bitbucketRoot")
  jiraRoot <- run GitEnv (GetConfigItem "jiraRoot")
  let env = Env { username = username
                , password = password
                , jiraRoot = jiraRoot
                , bitbucketRoot = bitbucketRoot
                }
  writeIORef globalEnv env
  return env

request_ :: (JSONResource json res) => res -> (Env -> String) -> IO json
request_ res rootGetter = do
  env@Env{..} <- readIORef globalEnv
  let ro = RequestOptions { authOptions = A.BasicAuth { A.username = username
                                                      , A.password = password
                                                      }
                          , resourceRoot = rootGetter env
                          }
  getJSON ro res


bitbucketRequest :: (BitbucketResource res, JSONResource json res) => res -> IO json
bitbucketRequest res = request_ res bitbucketRoot

jiraRequest :: (JIRAResource res, JSONResource json res) => res -> IO json
jiraRequest res = request_ res jiraRoot

renderWithSeqNum :: forall a. Show a => [a] -> (a -> String) -> IO ()
renderWithSeqNum objs showFunc = do
  zipWithM_ render objs [1..]
  where
    render :: a -> Int -> IO ()
    render obj seqNum =
      printf "[%d]\t%s\n" seqNum (showFunc obj)

ensureBitbucketProjectExists :: String  -- ^ project key
                             -> IO ()
ensureBitbucketProjectExists key = do
  res <- bitbucketRequest RB.GetProjectList
  unless (key `elem` (map RB.projectKey (RB.projects res))) $
    throwM $ InvalidBitbucketProject (printf "Project does not exist: %s" key)
setActiveBitbucketProject :: String  -- ^ project key
                          -> IO ()
setActiveBitbucketProject key = do
  ensureBitbucketProjectExists key
  run GitEnv (SetConfigItem "activeBitbucketProject" key)


ensureJIRAProjectExists :: String  -- ^ project key
                        -> IO ()
ensureJIRAProjectExists key = do
  res <- jiraRequest RJ.GetProjectList
  unless (key `elem` (map RJ.projectKey (RJ.projects res))) $
    throwM $ InvalidJIRAProject (printf "Project does not exist: %s" key)


setActiveJIRAProject :: String  -- ^ project key
                     -> IO ()
setActiveJIRAProject key = do
  ensureJIRAProjectExists key
  run GitEnv (SetConfigItem "activeJIRAProject" key)


ensureIssueExists :: String  -- ^ issue key
                  -> IO ()
ensureIssueExists key = do
  void $ jiraRequest RJ.GetIssue{ getIssueKey = key } `catch` handler
  where
    handler NotFound = do
      throwM $ InvalidJIRAIssue (printf "Issue does not exist: %s" key)

workonIssue :: String  -- ^ issue key
            -> IO ()
workonIssue key = do
  ensureIssueExists key
  putStrLn "Not implemented yet _(:з」∠)_"
