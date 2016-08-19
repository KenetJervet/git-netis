{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitNetis.App.Util where

import           Control.Monad
import           Data.ByteString.Lazy   (ByteString)
import           Data.IORef
import           GitNetis.App
import           GitNetis.App.Env
import           GitNetis.Resource
import           GitNetis.Resource.Auth as A
import           Text.Printf

baseRequest_ :: (Resource method res) => res
             -> (Env -> String)
             -> (RequestOptions -> res -> IO a)
             -> IO a
baseRequest_ res rootGetter respGetter = do
  env@Env{..} <- readIORef globalEnv
  let ro = RequestOptions { authOptions = A.BasicAuth { A.username = username
                                                      , A.password = password
                                                      }
                          , resourceRoot = rootGetter env
                          }
  respGetter ro res

request :: (Resource method res) => res -> (Env -> String) -> IO ByteString
request res rootGetter = baseRequest_ res rootGetter get

requestJSON :: (JSONResource json method res) => res -> (Env -> String) -> IO json
requestJSON res rootGetter = baseRequest_ res rootGetter getJSON


inform :: String -> IO ()
inform = putStrLn

renderWithSeqNum :: forall a. Show a => [a] -> (a -> String) -> IO ()
renderWithSeqNum objs showFunc = do
  zipWithM_ render objs [1..]
  where
    render :: a -> Int -> IO ()
    render obj seqNum =
      printf "[%d]\t%s\n" seqNum (showFunc obj)
