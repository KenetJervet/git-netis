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

------------------------
-- Informing and Prompts
------------------------

data InputMode = PlainText | Password

inform :: String  -- ^ message
       -> IO ()
inform = putStrLn

baseInform :: String -> IO ()
baseInform msg = putStr msg >> putChar ' ' >> hFlush stdout

basePrompt :: String  -- ^ message
           -> InputMode
           -> IO String
basePrompt msg mode = do
  baseInform msg
  case mode of
    PlainText -> getLine
    Password ->
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

prompt :: String  -- ^ message
       -> IO String
prompt = flip basePrompt PlainText

validatedPrompt :: String  -- ^ message
                -> (String -> IO Bool)  -- ^ predicate
                -> InputMode
                -> IO String
validatedPrompt msg pred mode = do
  val <- basePrompt msg mode
  result <- pred val
  if result then return val else validatedPrompt msg pred mode

promptWithDefault :: String  -- ^ message
                  -> String  -- ^ default
                  -> IO String
promptWithDefault msg def = do
  val <- basePrompt [i|#{msg} [#{def}]:|] PlainText
  return $ if val == "" then def else val

validatedPromptWithDefault :: String  -- ^ message
                           -> String  -- ^ default
                           -> (String -> IO Bool)  -- ^ predicate
                           -> IO String
validatedPromptWithDefault msg def pred = do
  val <- promptWithDefault msg def
  result <- pred val
  if result then return val else validatedPromptWithDefault msg def pred

promptYesNo :: String -> Bool -> IO Bool
promptYesNo msg def = do
  val <- basePrompt [i|#{msg} [#{upY}/#{upN}]|] PlainText
  if val == "" then return def else toBool val
    where
      upY = if def then 'Y' else 'y'
      upN = if def then 'n' else 'N'
      toBool val
        | val == "Y" || val == "y" = return True
        | val == "N" || val == "n" = return False
        | otherwise = promptYesNo msg def

-- OK
promptPassword :: String  -- ^ message
               -> IO String
promptPassword msg = basePrompt msg Password

validatedPromptPassword :: String  -- ^ message
                        -> (String -> IO Bool)  -- ^ predicate
                        -> IO String
validatedPromptPassword msg pred = do
  val <- promptPassword msg
  result <- pred val
  inform ""
  if result then return val else validatedPromptPassword msg pred

-- Validation helpers
required :: String -> IO Bool
required = return . not . null

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
