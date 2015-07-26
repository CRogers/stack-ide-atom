{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module NiceChildProcess (ChildProcess(..), spawn, NiceChildProcessException) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Debug.Trace
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import Data.Typeable (Typeable)
import GHCJS.Types
import GHCJS.Foreign

import qualified Node.ChildProcess as CP

data NiceChildProcessException
  = NoSuchFile Text
  deriving (Eq, Show, Typeable)

instance Exception NiceChildProcessException

type Command = Text
type Arg = Text
type Directory = Text

type Line = Text
type LineQueue = Chan Line

data ChildProcess = ChildProcess {
  readLine :: IO Text,
  writeLine :: Text -> IO ()
}

data ChildProcessData = ChildProcessData CP.ChildProcess (MVar NiceChildProcessException) LineQueue

createException :: Text -> NiceChildProcessException
createException = NoSuchFile

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = do
  childProcess <- CP.spawn (toJSString command) (map toJSString args) (toJSString cwd)
  errored <- newEmptyMVar
  CP.onError childProcess $ \err ->
    putMVar errored (createException $ fromJSString (CP.errorMessage err))
  outStream <- CP.stdout childProcess
  lineQueue <- newChan
  CP.onData outStream $ \buffer -> do
    text <- fromJSString <$> CP.toString buffer
    let lines = T.lines text
    void $ traverse (writeChan lineQueue) lines
  let childProcessData = ChildProcessData childProcess errored lineQueue
  return $ ChildProcess (makeReadLine childProcessData) (makeWriteLine childProcessData)

raceTo :: MVar NiceChildProcessException -> IO a -> IO a
raceTo errored other = do
  winner <- race (readMVar errored) other
  case winner of
    Left exception -> throw exception
    Right value -> return value

makeReadLine :: ChildProcessData -> IO Text
makeReadLine (ChildProcessData childProcess errored lineQueue) = do
  line <- raceTo errored (readChan lineQueue)
  traceShow line (return line)

makeWriteLine :: ChildProcessData -> Text -> IO ()
makeWriteLine (ChildProcessData childProcess errored _) text = do
  traceShow text (return ())
  hadErrored <- tryReadMVar errored
  case hadErrored of
    Nothing -> return ()
    Just exception -> throw exception

  inStream <- CP.stdin childProcess
  CP.write inStream (toJSString $ T.snoc text '\n')