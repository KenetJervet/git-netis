module GitNetis.Resource.JIRA where

import GitNetis.Resource


data ProjectList = ProjectList

instance Resource ProjectList where
  uri _ = "project"
