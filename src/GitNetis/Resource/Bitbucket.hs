{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass #-}

module GitNetis.Resource.Bitbucket where

import           Data.Aeson
import           GHC.Generics
import           GitNetis.Resource

class Resource res => BitbucketResource res

data GetProjectList = GetProjectList deriving BitbucketResource

instance Resource GetProjectList where
  uri _ = "projects"

data Project = Project { projectKey         :: String
                       , projectDescription :: String
                       } deriving Show

data ProjectList = ProjectList{ values :: [Project] } deriving Show

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    description <- obj .: "description"
    return Project{ projectKey = key,projectDescription = description }

instance FromJSON ProjectList where
  parseJSON = withObject "Bitbucket Rest API returned object" $ \obj -> do
    values <- obj .: "values"
    return ProjectList{ values = values }

instance JSONResource ProjectList GetProjectList
