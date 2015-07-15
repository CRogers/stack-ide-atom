{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHCJS.Types (JSString)
import Node.ChildProcess

foreign import javascript unsafe
  "console.log($1.toString())" consoleLog :: Buffer -> IO ()

main :: IO ()
main = do
  childProcess <- spawn "echo" ["lel"] "."
  stream <- stdout childProcess
  on stream "data" consoleLog
