{-# LANGUAGE OverloadedStrings #-}

module StackIde where

import Control.Monad.Free
import Stack.Ide.JsonAPI

import StackIdeM
import NiceChildProcess

runStackIde :: StackIdeM a -> IO a
runStackIde = iterM run where
  run :: StackIde (IO a) -> IO a
  run stackIde = case stackIde of
    CreateSession directory next -> do
      childProcess <- spawn "stack" ["ide"] directory
      version <- readLine childProcess
      putStrLn (show version)
      next

    GetVersion f -> do
      putStrLn "verion yay"
      f undefined
