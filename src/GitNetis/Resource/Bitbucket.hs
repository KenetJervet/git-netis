module GitNetis.Resource.Bitbucket where

import           Data.Aeson
import           GHC.Generics
import           GitNetis.Resource
import           Text.Printf

class Resource method res => BitbucketResource method res

-------------
-- Data decls
-------------

data Project = Project { projectKey         :: String
                       , projectDescription :: String
                       } deriving Show

data ProjectList = ProjectList { projects :: [Project]
                               } deriving Show

data Repo = Repo { repoName       :: String
                 , repoProjectKey :: String
                 }

data RepoList = RepoList { repos :: [Repo]
                         }


------------
-- Resources
------------

data GetProjectList = GetProjectList
data GetRepoList = GetRepoList { getRepoProjectKey :: String
                               }


---------------------
-- Resource instances
---------------------

instance Resource HttpGet GetProjectList where
  uri _ = return "projects"

instance Resource HttpGet GetRepoList where
  uri GetRepoList{..} = return $ printf "projects/%s/repos" getRepoProjectKey


-----------------
-- JSON instances
-----------------

instance FromJSON Project where
  parseJSON = withObject "project object" $ \obj -> do
    key <- obj .: "key"
    description <- obj .: "description"
    return Project{ projectKey = key, projectDescription = description }

instance FromJSON ProjectList where
  parseJSON = withObject "project list object" $ \obj -> do
    values <- obj .: "values"
    return ProjectList{ projects = values }

instance FromJSON Repo where
  parseJSON = withObject "repo object" $ \obj -> do
    name <- obj .: "name"
    projectKey <- obj .: "project" >>= (.: "key")
    return Repo{ repoName = name, repoProjectKey = projectKey }

instance FromJSON RepoList where
  parseJSON = withObject "repo list object" $ \obj -> do
    values <- obj .: "values"
    return RepoList{ repos = values }


--------------------------
-- JSON resource instances
--------------------------

instance JSONResource ProjectList HttpGet GetProjectList

instance JSONResource RepoList HttpGet GetRepoList


-------------------------------
-- Bitbucket resource instances
-------------------------------

instance BitbucketResource HttpGet GetProjectList

instance BitbucketResource HttpGet GetRepoList
