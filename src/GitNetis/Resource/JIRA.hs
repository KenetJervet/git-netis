{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GitNetis.Resource.JIRA where

import           Data.Aeson
import qualified Data.Vector       as V
import           GHC.Generics
import           GitNetis.Resource

class Resource res => JIRAResource res

data Project = Project { projectKey  :: String
                       , projectName :: String
                       } deriving Show

newtype ProjectList = ProjectList { projects :: [Project]
                                  } deriving Show

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    name <- obj .: "name"
    return Project{ projectKey = key,projectName = name }

instance FromJSON ProjectList where
  parseJSON = withArray "project objects" $ \obj ->
    ProjectList <$> (mapM parseJSON . V.toList) obj

data GetProjectList = GetProjectList deriving JIRAResource

instance Resource GetProjectList where
  uri _ = "project"

instance JSONResource ProjectList GetProjectList
