{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module GitNetis.Resource.Bitbucket where

import           Data.Aeson
import           GHC.Generics
import           GitNetis.Resource

class Resource method res => BitbucketResource method res

-------------
-- Data decls
-------------

data Project = Project { projectKey         :: String
                       , projectDescription :: String
                       } deriving Show

data ProjectList = ProjectList{ projects :: [Project] } deriving Show


------------
-- Resources
------------

data GetProjectList = GetProjectList


---------------------
-- Resource instances
---------------------

instance Resource HttpGet GetProjectList where
  uri _ = return "projects"


-----------------
-- JSON instances
-----------------

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    description <- obj .: "description"
    return Project{ projectKey = key, projectDescription = description }

instance FromJSON ProjectList where
  parseJSON = withObject "Bitbucket Rest API returned object" $ \obj -> do
    values <- obj .: "values"
    return ProjectList{ projects = values }


--------------------------
-- JSON resource instances
--------------------------

instance JSONResource ProjectList HttpGet GetProjectList


-------------------------------
-- Bitbucket resource instances
-------------------------------

instance BitbucketResource HttpGet GetProjectList
