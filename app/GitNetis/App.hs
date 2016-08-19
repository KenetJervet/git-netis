{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitNetis.App where

import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString.Lazy             (ByteString)
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

baseRequest_ :: (Resource method res) => res
         -> (Env -> String)
         -> (RequestOptions -> res -> IO a)
         -> IO a
baseRequest_ res rootGetter respGetter = do
  env@Env{..} <- readIORef globalEnv
  let ro = RequestOptions { authOptions = A.BasicAuth { A.username = username
                                                      , A.password = password
                                                      }
                          , resourceRoot = rootGetter env
                          }
  respGetter ro res

request_ :: (Resource method res) => res -> (Env -> String) -> IO ByteString
request_ res rootGetter = baseRequest_ res rootGetter get

requestJSON_ :: (JSONResource json method res) => res -> (Env -> String) -> IO json
requestJSON_ res rootGetter = baseRequest_ res rootGetter getJSON


bitbucketRequest :: (BitbucketResource method res, Resource method res)
                 => res
                 -> IO ByteString
bitbucketRequest res = request_ res bitbucketRoot

bitbucketRequestJSON :: (BitbucketResource method res, JSONResource json method res)
                 => res
                 -> IO json
bitbucketRequestJSON res = requestJSON_ res bitbucketRoot


jiraRequest :: (JIRAResource method res, Resource method res)
                 => res
                 -> IO ByteString
jiraRequest res = request_ res jiraRoot

jiraRequestJSON :: (JIRAResource method res, JSONResource json method res) => res -> IO json
jiraRequestJSON res = requestJSON_ res jiraRoot

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
  res <- bitbucketRequestJSON RB.GetProjectList
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
  res <- jiraRequestJSON RJ.GetProjectList
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
  void $ jiraRequestJSON RJ.GetIssue{ getIssueKey = key } `catch` handler
  where
    handler NotFound = do
      throwM $ InvalidJIRAIssue (printf "Issue does not exist: %s" key)

workonIssue :: String  -- ^ issue key
            -> IO ()
workonIssue key = do
  ensureIssueExists key
  void $ jiraRequest RJ.WorkonIssue { workonIssueKey = key }
