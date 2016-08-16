module GitNetis.Resource.Bitbucket where

import GitNetis.Resource

data GetProjectList = GetProjectList

instance Resource GetProjectList where
  uri _ = "projects"
