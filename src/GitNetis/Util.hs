module GitNetis.Util where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.HashMap.Lazy as H

packString :: String -> ByteString
packString = T.encodeUtf8 . T.pack

packText :: Text -> ByteString
packText = T.encodeUtf8

lookupE :: Value -> Text -> Either String Value
lookupE (Object obj) key = case H.lookup key obj of
        Nothing -> Left $ "key " ++ show key ++ " not present"
        Just v  -> Right v
loopkupE _ _             = Left $ "not an object"

(.:*) :: (FromJSON a) => Value -> [Text] -> Parser a
(.:*) value = parseJSON <=< foldM ((either fail return .) . lookupE) value

(.:?*) :: (FromJSON a) => Value -> [Text] -> Parser (Maybe a)
(.:?*) value = either (\_ -> return Nothing) (liftM Just . parseJSON)
                . foldM lookupE value
