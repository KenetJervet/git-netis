module GitNetis.Resource.Auth ( AuthOptions (..)
                              , NoAuth (..)
                              , BasicAuth (..)
                              , OAuth2 (..)
                              ) where

import           Control.Lens
import           Data.ByteString           as BS
import           Data.ByteString.Base64
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 as T
import           GitNetis.Util
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status as HTTP
import           Network.Wreq              as Wreq

class AuthOptions a where
  applyAuth :: a -> Options -> IO Options

----------
-- No auth
----------

data NoAuth = NoAuth

instance AuthOptions NoAuth where
  applyAuth _ = return

-------------
-- Basic auth
-------------

data BasicAuth = BasicAuth { username :: String, password :: String }

instance AuthOptions BasicAuth where
  applyAuth BasicAuth{..} options =
    return $ options & auth .~
    (
      Just $ basicAuth (packText $ T.pack username) (packText $ T.pack password)
    )

---------
-- OAuth2
---------

data OAuth2 = OAuth2 { token :: Text }

instance AuthOptions OAuth2 where
  applyAuth OAuth2{..} options =
    return $ options & auth .~
    (
      Just $ oauth2Bearer (packText token)
    )
