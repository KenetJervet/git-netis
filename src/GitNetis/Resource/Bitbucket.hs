module GitNetis.Resource.Bitbucket where

import GitNetis.Resource

data ProjectList = ProjectList

instance Resource ProjectList where
  uri _ = "projects"
