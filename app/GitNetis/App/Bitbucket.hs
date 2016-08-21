{-# LANGUAGE MultiParamTypeClasses #-}

module GitNetis.App.Bitbucket where

import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString.Lazy        (ByteString)
import           GitNetis.App.Env
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


