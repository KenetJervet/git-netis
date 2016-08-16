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

data TestConfig = TestConfig { mycred :: MyCredConfig
                             } deriving (Show, Generic, FromJSON)

data MyCredConfig = MyCredConfig { username :: Text
                                 , password :: Text
                                 } deriving (Show, Generic, FromJSON)

readConfig :: FilePath -> IO TestConfig
readConfig = LBS.readFile >=> return . fromJust . decode

{-# NOINLINE globalConfig #-}
globalConfig :: IORef TestConfig
globalConfig = unsafePerformIO $ newIORef TestConfig{}
