{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module ChildProcessSpec where

import Control.Applicative
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

xit _ _ = return ()

spec = do
  describe "Node.ChildProcess should" $ do
    it "spawn a process correctly" $ do
      childProcess <- spawn "echo" ["lel"] "."
      stream <- stdout childProcess
      sema <- newEmptyMVar
      onData stream $ \buffer -> do
        str <- toString buffer
        putMVar sema str
      value <- takeMVar sema
      value `shouldBe` "lel\n"

    it "set the cwd properly" $ do
      childProcess <- spawn "pwd" [] "/etc"
      stream <- stdout childProcess
      sema <- newEmptyMVar
      onData stream $ \buffer -> do
        str <- toString buffer
        putMVar sema str
      value <- takeMVar sema
      value `shouldBe` "/etc\n"

    it "not produce output line by line" $ do
      childProcess <- spawn "cat" ["test-data/multiline-file"] "."
      stream <- stdout childProcess
      sema <- newEmptyMVar
      onData stream $ \buffer -> do
        str <- toString buffer
        putMVar sema str
      value <- takeMVar sema
      value `shouldBe` "line one\nline two\n"

    it "error when no executable exists" $ do
      childProcess <- spawn "blahblah" [] "."
      sema <- newEmptyMVar
      onError childProcess $ \err -> do
        putMVar sema ()
      takeMVar sema

    it "correctly output to stdin" $ do
      childProcess <- spawn "cat" [] "."
      sema <- newEmptyMVar
      streamOut <- stdout childProcess
      onData streamOut $ \buffer ->
        putMVar sema =<< toString buffer
      streamIn <- stdin childProcess
      streamIn `write` "hello"
      value <- takeMVar sema
      value `shouldBe` "hello"
