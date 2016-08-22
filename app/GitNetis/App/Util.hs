{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitNetis.App.Util where

import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy   (ByteString)
import           Data.IORef
import           GitNetis.App
import           GitNetis.App.Env
import           GitNetis.Resource
import           GitNetis.Resource.Auth as A
import           System.IO
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


inform :: PrintfType r => String -> r
inform s = printf (s ++ "\n")

prompt :: String -> IO String  -- ^ message
prompt msg = printf msg >> putChar ' ' >> hFlush stdout >> getLine

-- OK
promptPassword :: String  -- ^ message
               -> IO String
promptPassword msg = do
  putStr msg
  putChar ' '
  hFlush stdout
  bracket open close (\_ -> getLine)
  where
    open = do
      b <- hGetBuffering stdin
      e <- hGetEcho stdin
      hSetBuffering stdin NoBuffering
      hSetEcho      stdin False
      return (b, e)
    close (b, e) = do
      hSetBuffering stdin b
      hSetEcho      stdin e

renderWithSeqNum :: forall a. Show a => [a] -> (a -> String) -> String
renderWithSeqNum objs showFunc = do
  join $ zipWith render objs [1..]
  where
    render :: a -> Int -> String
    render obj seqNum =
      printf "[%d]\t%s\n" seqNum (showFunc obj)
