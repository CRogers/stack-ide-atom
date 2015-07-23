{-# LANGUAGE OverloadedStrings #-}

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
import NiceChildProcess

data State = State (MVar ChildProcess) (MVar VersionInfo)

runStackIde :: StackIdeM a -> IO a
runStackIde stackIde = do
  version <- newEmptyMVar
  childProcess <- newEmptyMVar
  iterM (run $ State childProcess version) stackIde

  where
    run :: State -> StackIde (IO a) -> IO a
    run (State childProcessStore version) stackIde = case stackIde of
      CreateSession directory next -> do
        childProcess <- spawn "stack" ["ide"] directory
        putMVar childProcessStore childProcess
        welcomeRepsonseStr <- readLine childProcess
        let (Just (ResponseWelcome versionInfo)) = decode (fromStrict $ encodeUtf8 welcomeRepsonseStr)
        putMVar version versionInfo
        next

      GetVersion f ->
        f =<< readMVar version

      GetSourceErrors f -> do
        childProcess <- readMVar childProcessStore
        writeLine childProcess (decodeUtf8 (toStrict (encode RequestGetSourceErrors)))
        sourceErrorsStr <- readLine childProcess
        let (Just sourceErrors) = decode (fromStrict $ encodeUtf8 sourceErrorsStr)
        f sourceErrors