module NiceChildProcessSpec where

import Test.Hspec

import GHCJS.Types

import NiceChildProcess

should = it
xshould _ _ = return ()

spec :: Spec
spec = do
  describe "NiceChildProcess should" $ do
    should "read a line from a process" $ do
      childProcess <- spawn "echo" ["lol"] "."
      line <- readLine childProcess
      line `shouldBe` "lol"

    should "read one line from a process if no newline has appeared" $ do
      childProcess <- spawn "cat" ["test-data/two-line-file-no-trailing-newline"] "."
      line1 <- readLine childProcess
      line1 `shouldBe` "line one"

    should "read two lines from a process" $ do
      childProcess <- spawn "cat" ["test-data/two-line-file-with-trailing-newline"] "."
      line1 <- readLine childProcess
      line2 <- readLine childProcess
      line1 `shouldBe` "line one"
      line2 `shouldBe` "line two"
