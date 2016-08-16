{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FunctionalDependencies #-}

module GitNetis.Resource ( RequestOptions (..)
                         , ResourceRequestError (..)
                         , Resource (..)
                         , JSONResource (..)
                         ) where

import           Control.Exception      as E
import           Control.Lens
import qualified Data.Aeson             as J
import           Data.ByteString.Lazy
import           Data.Either
import           Data.Maybe
import           GitNetis.Resource.Auth
import qualified Network.HTTP.Client    as N
import           Network.URI
import           Network.Wreq


type Result = Either

isSuccess :: Result e a -> Bool
isSuccess = isRight

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
  get :: RequestOptions
      -> res
      -> IO (Either ResourceRequestError ByteString)
  get = get_ return
  getValue :: RequestOptions
           -> res
           -> IO (Either ResourceRequestError J.Value)
  getValue = get_ asValue


class (J.FromJSON json, Resource res) => JSONResource json res | res -> json where
  getJSON :: (J.FromJSON json) =>
             RequestOptions
          -> res
          -> IO (Either ResourceRequestError json)
  getJSON = get_ asJSON
