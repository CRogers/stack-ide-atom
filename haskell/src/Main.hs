{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHCJS.Types (JSString)
import Node.ChildProcess

import Stack.Ide.JsonAPI

foreign import javascript unsafe
  "console.log($1.toString())" consoleLog :: JSString -> IO ()

main :: IO ()
main = do
  childProcess <- spawn "echo" ["lel"] "."
  stream <- stdout childProcess
  on stream "data" consoleLog
