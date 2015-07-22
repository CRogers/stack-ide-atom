{-# LANGUAGE OverloadedStrings #-}

module StackIde where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Free
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromChunks)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Stack.Ide.JsonAPI

import StackIdeM
import NiceChildProcess

data State = State (MVar VersionInfo)

runStackIde :: StackIdeM a -> IO a
runStackIde stackIde = do
  version <- newEmptyMVar
  iterM (run $ State version) stackIde

  where
    run :: State -> StackIde (IO a) -> IO a
    run (State version) stackIde = case stackIde of
      CreateSession directory next -> do
        childProcess <- spawn "stack" ["ide"] directory
        welcomeRepsonseStr <- readLine childProcess
        let (Just (ResponseWelcome versionInfo)) = decode (fromChunks [encodeUtf8 welcomeRepsonseStr])
        putMVar version versionInfo
        next

      GetVersion f ->
        f =<< readMVar version
