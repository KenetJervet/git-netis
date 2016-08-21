{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module GitNetis.Test.TestConfig where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy as LBS
import           Data.IORef
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import           System.IO
import           System.IO.Unsafe

data TestConfig = TestConfig { myCred :: MyCredConfig
                             , myJIRAConfig :: MyJIRAConfig
                             , myBitbucketConfig :: MyBitbucketConfig
                             } deriving (Show, Generic, FromJSON)

data MyCredConfig = MyCredConfig { username :: String
                                 , password :: String
                                 } deriving (Show, Generic, FromJSON)

data MyJIRAConfig = MyJIRAConfig { activeJIRAProject :: String
                                 } deriving (Show, Generic, FromJSON)

data MyBitbucketConfig = MyBitbucketConfig { activeBitbucketProject :: String
                                           } deriving (Show, Generic, FromJSON)

readConfig :: FilePath -> IO TestConfig
readConfig = LBS.readFile >=> return . fromJust . decode

{-# NOINLINE globalConfig #-}
globalConfig :: IORef TestConfig
globalConfig = unsafePerformIO $ newIORef TestConfig{}
