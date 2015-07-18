{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module ChildProcessSpec where

import Control.Monad
import Control.Concurrent.MVar

import Control.Concurrent
import Test.Hspec
import GHCJS.Types
import GHCJS.Foreign

import Node.ChildProcess

instance Show JSString where
  show = fromJSString

foreign import javascript unsafe
  "$1 === $2" js_str_eq :: JSString -> JSString -> JSBool

instance Eq JSString where
  str1 == str2 = fromJSBool (str1 `js_str_eq` str2)

spec = do
  describe "Node.ChildProcess" $ do
    it "should spawn a process correctly" $ do
      childProcess <- spawn "echo" ["lel"] "."
      stream <- stdout childProcess
      sema <- newEmptyMVar
      on stream "data" $ \buffer -> do
        str <- toString buffer
        putMVar sema str
      value <- takeMVar sema
      value `shouldBe` "lel\n"
