{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitNetis.Resource ( RequestOptions (..)
                         , ResourceRequestError (..)
                         , Resource (..)
                         ) where

import           Control.Exception    as E
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Maybe
import           GitNetis.Resource.Auth
import qualified Network.HTTP.Client  as N
import           Network.URI
import           Network.Wreq


data RequestOptions where
  RequestOptions :: (AuthOptions ao) =>
                    { authOptions :: ao
                    , resourceRoot :: String
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
       -> IO (Either ResourceRequestError asType)
  get_ as ro res = do
    let rootUri = fromJust $ parseURI (resourceRoot ro)
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
          -> IO (Either ResourceRequestError json)
  getJSON = get_ asJSON
  getValue :: RequestOptions
           -> res
           -> IO (Either ResourceRequestError Value)
  getValue = get_ asValue
