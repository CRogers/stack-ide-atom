{-# LANGUAGE NoMonomorphismRestriction #-}

module UpdateSkippingStackIdeProcessSpec where

import Test.Hspec

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Stack.Ide.JsonAPI

import UpdateSkippingStackIdeProcess

should = it
xshould _ _ = return ()

spec :: Spec
spec =
  describe "StackIdeProcess should" $ do
    should "pass through RequestGetSourceErrors command" $ do
      requested <- newEmptyMVar
      let stackIdeProcess = StackIdeProcess (putMVar requested) (return undefined)
      let updateSkipping = createFromStackIdeProcess stackIdeProcess
      request updateSkipping RequestGetSourceErrors
      takeMVar requested >>= (`shouldBe` RequestGetSourceErrors)

    should "pass back a ResponseShutdownSession" $ do
      let stackIdeProcess = StackIdeProcess (const $ return ()) (return ResponseShutdownSession)
      let updateSkipping = createFromStackIdeProcess stackIdeProcess
      awaitResponse updateSkipping >>= (`shouldBe` ResponseShutdownSession)

    should "pass back only a ResponseShutdownSession with an UpdateSession then ShutdownSession" $ do
      responses <- newChan
      writeList2Chan responses [ResponseUpdateSession undefined, ResponseShutdownSession]
      let stackIdeProcess = StackIdeProcess (const $ return ()) (readChan responses)
      let updateSkipping = createFromStackIdeProcess stackIdeProcess
      awaitResponse updateSkipping >>= (`shouldBe` ResponseShutdownSession)

    should "pass back only a ResponseShutdownSession with an UpdateSession, UpdateSession, ShutdownSession" $ do
      responses <- newChan
      let updateSession = ResponseUpdateSession undefined
      writeList2Chan responses [updateSession, updateSession, ResponseShutdownSession]
      let stackIdeProcess = StackIdeProcess (const $ return ()) (readChan responses)
      let updateSkipping = createFromStackIdeProcess stackIdeProcess
      awaitResponse updateSkipping >>= (`shouldBe` ResponseShutdownSession)