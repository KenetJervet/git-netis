module GitNetis.JIRA where

import           Control.Lens
import           Data.ByteString.Lazy
import           Network.Wreq

baidu :: IO ByteString
baidu = do
  resp <- get "http://www.baidu.com"
  return $ resp ^. responseBody
