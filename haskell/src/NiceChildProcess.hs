{-# LANGUAGE OverloadedStrings #-}

module NiceChildProcess where

import Control.Concurrent.MVar
import Control.Applicative
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

newtype ChildProcess = ChildProcess CP.ChildProcess

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = ChildProcess <$> CP.spawn (toJSString command) (map toJSString args) (toJSString cwd)

readLine :: ChildProcess -> IO Text
readLine (ChildProcess childProcess) = do
  outStream <- CP.stdout childProcess
  sema <- newEmptyMVar
  CP.onData outStream $ \buffer -> do
    str <- CP.toString buffer
    putMVar sema (T.dropWhileEnd (== '\n') $ fromJSString str)
  takeMVar sema
