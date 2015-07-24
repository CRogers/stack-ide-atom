{-# LANGUAGE NoMonomorphismRestriction #-}

module StackIdeProcessSpec where

import Test.Hspec

import Control.Concurrent.MVar
import Control.Monad
import Stack.Ide.JsonAPI

import NiceChildProcess
import StackIdeProcess

should = it
xshould _ _ = return ()

spec :: Spec
spec =
  describe "StackIdeProcess should" $ do
    should "correctly serialise a RequestGetSourceErrors" $ do
      written <- newEmptyMVar
      let childProcess = ChildProcess (return "") (\text -> do putMVar written text; return ())
      let stackIdeProcess = createFromChildProcess childProcess
      request stackIdeProcess RequestGetSourceErrors
      takeMVar written >>= (`shouldBe` "{\"tag\":\"RequestGetSourceErrors\",\"contents\":[]}")