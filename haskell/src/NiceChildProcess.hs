{-# LANGUAGE OverloadedStrings #-}

module NiceChildProcess where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import GHCJS.Types
import GHCJS.Foreign

import qualified Node.ChildProcess as CP

type Command = Text
type Arg = Text
type Directory = Text

data Line = Line Text | Error Text
type LineQueue = Chan Line

data ChildProcess = ChildProcess CP.ChildProcess LineQueue

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = do
  childProcess <- CP.spawn (toJSString command) (map toJSString args) (toJSString cwd)
  lineQueue <- newChan
  CP.onError childProcess $ \err -> do
    writeChan lineQueue (Error $ fromJSString (CP.errorMessage err))
  outStream <- CP.stdout childProcess
  CP.onData outStream $ \buffer -> do
    text <- fromJSString <$> CP.toString buffer
    let lines = T.lines text
    void $ traverse (writeChan lineQueue . Line) lines
  return $ ChildProcess childProcess lineQueue

readLine :: ChildProcess -> IO Text
readLine (ChildProcess childProcess lineQueue) = do
  line <- readChan lineQueue
  case line of
    Error msg -> error (show msg)
    Line text -> return text

writeLine :: ChildProcess -> Text -> IO ()
writeLine (ChildProcess childProcess _) text = do
  inStream <- CP.stdin childProcess
  CP.write inStream (toJSString $ T.snoc text '\n')