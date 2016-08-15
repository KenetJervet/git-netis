{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitNetis.JIRA.Resource ( RequestOptions (..)
                              , ResourceRequestError (..)
                              , Resource (..)
                              , ProjectList (..)
                              ) where

import           Control.Exception    as E
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Maybe
import           GitNetis.JIRA
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


data ResourceRequestError = AuthFailed
                          | NotFound
                          | IDontCare
                          deriving (Eq, Show)

class Resource res where
  uri :: res  -- ^ The resource
      -> String -- ^ Relative uri
  uri_ :: res -> URI
  uri_ = fromJust . parseRelativeReference . uri
  get_ :: (Response ByteString -> IO (Response asType))
       -> RequestOptions
       -> res
       -> String -- ^ Resource root
       -> IO (Either ResourceRequestError asType)
  get_ as ro res root = do
    let rootUri = fromJust $ parseURI root
    r <- as =<< requestWithOptions ro (rootUri { uriPath = uriPath rootUri ++ uri res})
    return $ Right (r ^. responseBody)
    `catch` handler
      where
        handler e@(N.StatusCodeException s _ _) =
          case s ^. statusCode of
            sc | sc == 401 -> return $ Left AuthFailed
               | sc == 404 -> return $ Left NotFound
               | otherwise -> return $ Left IDontCare
        handler e = error (show e)
  getJSON :: (FromJSON json) =>
             RequestOptions
          -> res
          -> String
          -> IO (Either ResourceRequestError json)
  getJSON = get_ asJSON
  getValue :: RequestOptions
           -> res
           -> String
           -> IO (Either ResourceRequestError Value)
  getValue = get_ asValue

-----------
-- Projects
-----------

data ProjectList = ProjectList

instance Resource ProjectList where
  uri _ = "project"
