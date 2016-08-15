{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitNetis.JIRA.Resource where

import           Control.Exception    as E
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Maybe
import           GitNetis.JIRA.Auth
import qualified Network.HTTP.Client  as N
import           Network.URI
import           Network.Wreq

data RequestOptions where
  RequestOptions :: (AuthOptions ao) =>
                    { authOptions :: ao
                    } -> RequestOptions

applyOptions :: RequestOptions -> Options -> IO Options
applyOptions RequestOptions{ authOptions = ao } = applyAuth ao

requestWithOptions :: RequestOptions -> URI -> IO (Response ByteString)
requestWithOptions ro uri = do
  options <- applyOptions ro defaults
  getWith options (uriToString id uri "")

data ResourceError = AuthFailed
                   | IDontCare

class Resource a where
  uri :: a  -- ^ The resource
      -> String -- ^ Relative uri
  uri_ :: a -> URI
  uri_ = fromJust . parseRelativeReference . uri
  getJSON :: (FromJSON json) =>
             RequestOptions
          -> a
          -> IO (Either ResourceError json)
  getJSON ro a = do
    r <- (asJSON =<< requestWithOptions ro (uri_ a))
    return $ Right (r ^. responseBody)
    `catch` handler
      where
        handler e@(N.StatusCodeException s _ _)
          | s ^. statusCode == 401 = return $ Left AuthFailed
          | otherwise = return $ Left IDontCare
