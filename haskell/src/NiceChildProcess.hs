{-# LANGUAGE OverloadedStrings #-}

module NiceChildProcess (ChildProcess(..), spawn) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
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
type LineQueue = Chan Line

newtype Errored = Errored Text

data ChildProcess = ChildProcess {
  readLine :: IO Text,
  writeLine :: Text -> IO ()
}

data ChildProcessData = ChildProcessData CP.ChildProcess (MVar Errored) LineQueue

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = do
  childProcess <- CP.spawn (toJSString command) (map toJSString args) (toJSString cwd)
  errored <- newEmptyMVar
  CP.onError childProcess $ \err ->
    putMVar errored (Errored $ fromJSString (CP.errorMessage err))
  outStream <- CP.stdout childProcess
  lineQueue <- newChan
  CP.onData outStream $ \buffer -> do
    text <- fromJSString <$> CP.toString buffer
    let lines = T.lines text
    void $ traverse (writeChan lineQueue) lines
  let childProcessData = ChildProcessData childProcess errored lineQueue
  return $ ChildProcess (makeReadLine childProcessData) (makeWriteLine childProcessData)

raceTo :: MVar Errored -> IO a -> IO a
raceTo errored other = do
  winner <- race (readMVar errored) other
  case winner of
    Left (Errored err) -> error $ show err
    Right value -> return value

makeReadLine :: ChildProcessData -> IO Text
makeReadLine (ChildProcessData childProcess errored lineQueue) = do
  raceTo errored (readChan lineQueue)

makeWriteLine :: ChildProcessData -> Text -> IO ()
makeWriteLine (ChildProcessData childProcess errored _) text = do
  hadErrored <- tryReadMVar errored
  case hadErrored of
    Nothing -> return ()
    Just (Errored err) -> error $ show err

  inStream <- CP.stdin childProcess
  CP.write inStream (toJSString $ T.snoc text '\n')