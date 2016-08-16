{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Resource.JIRA where

import GitNetis.Resource
import Data.Aeson
import qualified Data.Vector as V

data Project = Project { projectKey :: String
                       , projectDescription :: String
                       }

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    description <- obj .: "description"
    return Project{ projectKey = key,projectDescription = description }

data GetProjectList = GetProjectList

instance Resource GetProjectList where
  uri _ = "project"
