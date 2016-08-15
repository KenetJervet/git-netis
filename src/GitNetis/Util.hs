module GitNetis.Util where

import           Data.ByteString
import           Data.Text
import           Data.Text.Encoding as T

packText :: Text -> ByteString
packText = T.encodeUtf8
