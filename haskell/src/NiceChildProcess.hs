{-# LANGUAGE OverloadedStrings #-}

module NiceChildProcess where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Types
import GHCJS.Foreign

import qualified Node.ChildProcess as CP

type Command = Text
type Arg = Text
type Directory = Text

type LineBuffer = IORef [Text]
type Semaphore = MVar ()

data ChildProcess = ChildProcess CP.ChildProcess LineBuffer Semaphore

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = do
  childProcess <- CP.spawn (toJSString command) (map toJSString args) (toJSString cwd)
  outStream <- CP.stdout childProcess
  lineBuffer <- newIORef []
  sema <- newEmptyMVar
  CP.onData outStream $ \buffer -> do
    text <- fromJSString <$> CP.toString buffer
    let pieces = T.lines text
    modifyIORef' lineBuffer (<> pieces)
    replicateM_ (length pieces) $ putMVar sema ()
  return $ ChildProcess childProcess lineBuffer sema

readLine :: ChildProcess -> IO Text
readLine (ChildProcess childProcess lineBuffer sema) = do
  takeMVar sema
  atomicModifyIORef' lineBuffer (\(l:ls) -> (ls, l))

writeLine :: ChildProcess -> Text -> IO ()
writeLine (ChildProcess childProcess _ _) text = do
  inStream <- CP.stdin childProcess
  CP.write inStream (toJSString $ T.snoc text '\n')