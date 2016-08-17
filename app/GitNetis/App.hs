{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitNetis.App where

import           Control.Monad
import           Data.IORef
import           Data.Text                   as T
import           GitNetis.Git
import           GitNetis.Resource
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
