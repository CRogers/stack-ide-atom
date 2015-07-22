{-# LANGUAGE OverloadedStrings #-}

module StackIde where

import Control.Concurrent.MVar
import Control.Monad.Free
import Data.Text (Text)
import Stack.Ide.JsonAPI

import StackIdeM
import NiceChildProcess

data State = State (MVar Text)

runStackIde :: StackIdeM a -> IO a
runStackIde stackIde = do
  version <- newEmptyMVar
  iterM (run $ State version) stackIde

  where
    run :: State -> StackIde (IO a) -> IO a
    run (State version) stackIde = case stackIde of
      CreateSession directory next -> do
        childProcess <- spawn "stack" ["ide"] directory
        putMVar version =<< readLine childProcess
        next

      GetVersion f -> do
        putStrLn =<< fmap show (readMVar version)
        f undefined
