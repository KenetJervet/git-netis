{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module GitNetis.App.Bitbucket where

import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString.Lazy        (ByteString)
import           GitNetis.App.Env
import           GitNetis.App.Internal
import           GitNetis.App.Util
import           GitNetis.Git
import           GitNetis.Resource
import           GitNetis.Resource.Bitbucket
import           Text.Printf

data BitbucketError = InvalidBitbucketProject String
                    deriving Show

instance Exception BitbucketError


bitbucketRequest :: (BitbucketResource method res, Resource method res)
                 => res
                 -> IO ByteString
bitbucketRequest res = request res bitbucketRoot

bitbucketRequestJSON :: (BitbucketResource method res, JSONResource json method res)
                 => res
                 -> IO json
bitbucketRequestJSON res = requestJSON res bitbucketRoot


ensureBitbucketProjectExists :: String  -- ^ project key
                             -> IO ()
ensureBitbucketProjectExists key = do
  res <- bitbucketRequestJSON GetProjectList
  unless (key `elem` (map projectKey (projects res))) $
    throwM $ InvalidBitbucketProject (printf "Project does not exist: %s" key)
setActiveBitbucketProject :: String  -- ^ project key
                          -> IO ()
setActiveBitbucketProject key = do
  ensureBitbucketProjectExists key
  run GitEnv (SetConfigItem ActiveBitbucketProject key)


------------
-- Renderers
------------

type Renderer obj = obj -> String

render :: Renderer obj -> obj -> String
render f = f

class DefaultRenderer obj where
  def :: Renderer obj

instance DefaultRenderer Project where
  def Project{..} = printf "%s\t%s" projectKey projectDescription

instance DefaultRenderer ProjectList where
  def ProjectList{..} = renderWithSeqNum projects def


-----------------------------
-- Project bitbucket projects
-----------------------------

printProjects :: IO ()
printProjects = do
  res <- bitbucketRequestJSON GetProjectList
  putStr $ renderWithSeqNum (projects res) (render def)
