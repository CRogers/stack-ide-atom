{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module StackIde where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Free
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Stack.Ide.JsonAPI

import StackIdeM
import StackIdeProcess
import UpdateSkippingStackIdeProcess

data State = State (MVar StackIdeProcess) (MVar VersionInfo)

runStackIde :: StackIdeM a -> IO a
runStackIde stackIde = do
  version <- newEmptyMVar
  stackIdeProcess <- newEmptyMVar
  iterM (run $ State stackIdeProcess version) stackIde

  where
    run :: State -> StackIde (IO a) -> IO a
    run (State stackIdeProcessStore version) stackIde = case stackIde of
      CreateSession directory next -> do
        stackIdeProcess <- createUpdateSkippingStackIdeProcess directory
        putMVar stackIdeProcessStore stackIdeProcess
        (ResponseWelcome versionInfo) <- awaitResponse stackIdeProcess
        putMVar version versionInfo
        next

      GetVersion f ->
        f =<< readMVar version

      GetSourceErrors f -> do
        stackIdeProcess <- readMVar stackIdeProcessStore
        (ResponseGetSourceErrors sourceErrors) <- command stackIdeProcess RequestGetSourceErrors
        f sourceErrors