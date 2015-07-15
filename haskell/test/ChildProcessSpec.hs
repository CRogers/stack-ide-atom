{-# LANGUAGE OverloadedStrings #-}

module ChildProcessSpec where

import Control.Monad

import Control.Concurrent
import Test.Hspec
import GHCJS.Foreign

import Node.ChildProcess

spec = do
  describe "Node.ChildProcess" $ do
    it "should spawn a process correctly" $ do
      childProcess <- spawn "echo" ["lel"] "."
      stream <- stdout childProcess
      on stream "data" $ \buffer -> do
        str <- toString buffer
        putStrLn (fromJSString str)
      threadDelay $ 1000 * 1000
