{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE QuasiQuotes #-}

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
import           Data.String.Interpolate

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

prompt :: String -> IO String  -- ^ message
prompt msg = putStr msg >> putChar ' ' >> hFlush stdout >> getLine

defaultPrompt :: String -> String -> IO String
defaultPrompt msg defaultValue = do
  putStr [i|#{msg} [#{defaultValue}]: |]
  val <- getLine
  return $ if val == "" then defaultValue else val

-- OK
promptPassword :: String  -- ^ message
               -> IO String
promptPassword msg = do
  putStr msg
  putChar ' '
  hFlush stdout
  bracket open close (const getLine)
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
renderWithSeqNum objs showFunc =
  join $ zipWith render objs [1..]
  where
    render :: a -> Int -> String
    render obj seqNum =
      [i|[#{seqNum}]\t#{showFunc obj}\n|]
