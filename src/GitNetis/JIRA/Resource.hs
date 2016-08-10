{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GitNetis.JIRA.Resource where

data Project where
  Project :: { projectId :: (Integral int) => int
             , projectName :: String
             } -> Project
