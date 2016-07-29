{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts #-}

module GitNetis.JIRA where

import           Control.Lens
import           Data.ByteString           as BS
import           Data.ByteString.Base64
import qualified Data.ByteString.Lazy      as LBS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status as HTTP
import           Network.Wreq as Wreq

baidu :: IO ByteString
baidu = do
  resp <- get "http://www.baidu.com"
  return $ LBS.toStrict $ resp ^. responseBody

class JIRAAuth authMethod where
  type AuthOptions authMethod
  type AuthResult authMethod
  auth :: ( JIRAAuthOptions (AuthOptions authMethod)
          , JIRAAuthResult (AuthResult authMethod)) =>
          authMethod
       -> (AuthOptions authMethod)
       -> String -- ^URL
       -> IO (AuthResult authMethod)

class JIRAAuthOptions authOptions where
  username :: authOptions -> ByteString
  password :: authOptions -> ByteString

class JIRAAuthResult authResult where
  isOK :: authResult -> Bool

data GenericAuthOptions = GenericAuthOptions { gaoUsername :: ByteString
                                             , gaoPassword :: ByteString
                                             }

instance JIRAAuthOptions GenericAuthOptions where
  username = gaoUsername
  password = gaoPassword

type ErrorCode = Int

data GenericAuthResult = GenericAuthOK
                       | GenericAuthError { garErrorCode :: Int
                                          , garErrorMsg  :: ByteString
                                          }
                       deriving (Eq, Show)

instance JIRAAuthResult GenericAuthResult where
  isOK GenericAuthOK = True
  isOK _ = False

data BasicAuth = BasicAuth

instance JIRAAuth BasicAuth where
  type AuthOptions BasicAuth = GenericAuthOptions
  type AuthResult BasicAuth = GenericAuthResult
  auth BasicAuth GenericAuthOptions{..} url = do
    let options = defaults
                  & header hAuthorization .~ [mkAuthBS]
                  & header "Content-Type" .~ ["application/json"]
    resp <- getWith options url
    return $
      case resp ^. responseStatus of
        st | st >= ok200 && st < badRequest400 -> GenericAuthOK
           | otherwise -> GenericAuthError { garErrorCode = HTTP.statusCode st
                                           , garErrorMsg = HTTP.statusMessage st
                                           }
    return GenericAuthOK
    where
      mkAuthBS :: ByteString
      mkAuthBS = "Basic " `BS.append` b64UsernamePassword
      b64UsernamePassword = encode (gaoUsername `BS.append` ":" `BS.append` gaoPassword)
