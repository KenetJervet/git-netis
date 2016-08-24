{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module GitNetis.App.Util where

import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy    (ByteString)
import           Data.IORef
import           Data.List
import           Data.String.Interpolate
import           GitNetis.App
import           GitNetis.App.Env
import           GitNetis.Resource
import           GitNetis.Resource.Auth  as A
import           System.IO
import           Text.PrettyPrint.Boxes
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

data Showable = forall a. Show a => Showable a

renderTable :: [[String]] -> String
renderTable = render
              . punctuateH left (char ' ')
              . map (vcat left . map text)
              . transpose

renderTableWithHighlightedItem :: [a]  -- ^ items
                               -> (a -> [String])  -- ^ item renderer
                               -> (a -> Bool)  -- ^ predicate. True = highlight
                               -> String
renderTableWithHighlightedItem items showFunc pred =
  renderTable (prependHighlighter items $ prependSeqNum $ showAll items)
   where
     showAll = map showFunc
     prependSeqNum =
       zipWith (:) (map (printf "[%s]". show) [1..])
     prependHighlighter =
       zipWith $ (:) . (\obj -> if pred obj then ">" else "")
