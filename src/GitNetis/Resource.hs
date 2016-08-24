{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving     #-}

module GitNetis.Resource ( RequestOptions (..)
                         , ResourceRequestError (..)
                         , Resource (..)
                         , JSONResource (..)
                         , HttpGet
                         , HttpPost
                         , HttpPayload (..)
                         ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Catch
import qualified Data.Aeson             as J
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Either
import           Data.Maybe
import           GitNetis.Resource.Auth
import           GitNetis.Util
import qualified Network.HTTP.Client    as N
import           Network.URI
import           Network.Wreq           hiding (Payload)


data RequestOptions where
  RequestOptions :: (AuthOptions ao) =>
                    { authOptions :: ao
                    , resourceRoot :: String
                    } -> RequestOptions


applyOptions :: RequestOptions -> Options -> IO Options
applyOptions RequestOptions{ authOptions = ao } = applyAuth ao


data ResourceRequestError = AuthFailed
                          | NotFound
                          | forall a. Show a => IDontCare a

deriving instance Show ResourceRequestError

instance Exception ResourceRequestError


-- TODO: I would really benefit from TypeFamilyDependencies available in GHC 8.0

class HttpMethod a where
  makeRequest :: Options -> String -> HttpPayload a -> IO (Response ByteString)

data HttpGet
data HttpPost

data HttpPayload method where
  HttpGetPayload :: () -> HttpPayload HttpGet
  HttpPostFormPayload :: [(String, String)] -> HttpPayload HttpPost
  HttpPostJSONPayload :: (J.ToJSON json) => json -> HttpPayload HttpPost

instance HttpMethod HttpGet where
  makeRequest options uri _ = do
    getWith options uri

instance HttpMethod HttpPost where
  makeRequest options uri payload = case payload of
    HttpPostFormPayload payload ->
      postWith options uri $ map (uncurry (:=) . first packString) payload
    HttpPostJSONPayload payload ->
      postWith options uri (J.toJSON payload)

class HttpMethod method => Resource method res | res -> method where
  uri :: res
      -> IO String  -- ^ Relative uri
  payload :: res -> IO (HttpPayload method)

  default payload :: res -> IO (HttpPayload HttpGet)
  payload _ = return $ HttpGetPayload ()

  get_ :: (Response ByteString -> IO (Response asType))
       -> RequestOptions
       -> res
       -> IO asType
  get_ as ro res = do
    -- let rootUri = fromJust $ parseURI (resourceRoot ro)
    relUri <- uri res
    -- error $ resourceRoot ro
    options <- applyOptions ro defaults
    p <- payload res
    r <- as =<< makeRequest options (resourceRoot ro ++ relUri) p
    return $ r ^. responseBody
    `catch` handler
      where
        handler e@(N.StatusCodeException s _ _) =
          case s ^. statusCode of
            sc | sc == 401 -> throwM AuthFailed
               | sc == 404 -> throwM NotFound
               | otherwise -> throwM $ IDontCare e
        handler e = error (show e)
  get :: RequestOptions
      -> res
      -> IO ByteString
  get = get_ return
  getValue :: RequestOptions
           -> res
           -> IO J.Value
  getValue = get_ asValue


class (J.FromJSON json, Resource method res) =>
  JSONResource json method res | res -> json where
  getJSON :: J.FromJSON json =>
             RequestOptions
          -> res
          -> IO json
  getJSON = get_ asJSON
