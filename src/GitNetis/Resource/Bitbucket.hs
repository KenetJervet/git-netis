{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module GitNetis.Resource.Bitbucket where

import           Data.Aeson
import           GHC.Generics
import           GitNetis.Resource

data GetProjectList = GetProjectList

instance Resource GetProjectList where
  uri _ = "projects"

data Project = Project { projectKey         :: String
                       , projectDescription :: String
                       }

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    description <- obj .: "description"
    return Project{ projectKey = key,projectDescription = description }

instance JSONResource [Project] GetProjectList
