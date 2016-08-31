{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

module GitNetis.App.Bitbucket where

import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString.Lazy        (ByteString)
import           Data.String.Interpolate
import           GitNetis.App.Env
import           GitNetis.App.Internal
import           GitNetis.App.Util
import           GitNetis.Git
import           GitNetis.Resource
import           GitNetis.Resource.Bitbucket

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
  unless (key `elem` map projectKey (projects res)) $
    throwM $ InvalidBitbucketProject [i|Project does not exist: #{key}|]
setActiveBitbucketProject :: String  -- ^ project key
                          -> IO ()
setActiveBitbucketProject key = do
  ensureBitbucketProjectExists key
  run GitEnv (SetConfigItem ActiveBitbucketProject key)


-----------------------------
-- Project bitbucket projects
-----------------------------

printProjects :: [Project] -> IO ()
printProjects projects = do
  activeProject <- getWithDefault ActiveBitbucketProject ""
  putStrLn $
    renderTableWithHighlightedItem projects renderProject $
    (== activeProject) . projectKey
  where
    renderProject Project{..} = [projectKey, projectDescription]
