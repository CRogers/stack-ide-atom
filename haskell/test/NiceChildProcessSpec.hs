module NiceChildProcessSpec where

import Test.Hspec

import GHCJS.Types

import NiceChildProcess

should = it

spec :: Spec
spec = do
  describe "NiceChildProcess should" $ do
    should "read a line from a process" $ do
      childProcess <- spawn "echo" ["lol"] "."
      line <- readLine childProcess
      line `shouldBe` "lol"
