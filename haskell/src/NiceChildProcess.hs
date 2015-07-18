{-# LANGUAGE OverloadedStrings #-}

module NiceChildProcess where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
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

type Line = Text
type LineQueue = STM (TChan Line)

data ChildProcess = ChildProcess CP.ChildProcess LineQueue

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = do
  childProcess <- CP.spawn (toJSString command) (map toJSString args) (toJSString cwd)
  outStream <- CP.stdout childProcess
  let lineQueue = newTChan
  CP.onData outStream $ \buffer -> do
    text <- fromJSString <$> CP.toString buffer
    let lines = T.lines text
    void $ atomically $ traverse (\line -> writeTChan <$> lineQueue <*> pure line) lines
  return $ ChildProcess childProcess lineQueue

readLine :: ChildProcess -> IO Text
readLine (ChildProcess childProcess lineQueue) = do
  atomically (readTChan =<< lineQueue)

writeLine :: ChildProcess -> Text -> IO ()
writeLine (ChildProcess childProcess _) text = do
  inStream <- CP.stdin childProcess
  CP.write inStream (toJSString $ T.snoc text '\n')