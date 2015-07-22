module NiceChildProcessSpec where

import Test.Hspec

import Control.Concurrent
import GHCJS.Types

import NiceChildProcess

should = it
xshould _ _ = return ()

repeater :: IO ChildProcess
repeater = spawn "cat" [] "."

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

    should "read a line after writing a line" $ do
      childProcess <- repeater
      writeLine childProcess "foobar"
      line <- readLine childProcess
      line `shouldBe` "foobar"

    should "write a line, read a line, write a line, read a line" $ do
      childProcess <- repeater
      writeLine childProcess "foo"
      readLine childProcess >>= (`shouldBe` "foo")
      writeLine childProcess "bar"
      readLine childProcess >>= (`shouldBe` "bar")

    should "write a line, write a line, read a line, read a line" $ do
      childProcess <- repeater
      writeLine childProcess "foo"
      writeLine childProcess "bar"
      readLine childProcess >>= (`shouldBe` "foo")
      readLine childProcess >>= (`shouldBe` "bar")

    should "throw an error when executable can't be found when trying to readLine" $ do
      childProcess <- spawn "kasdhfskhdg" [] "."
      (readLine childProcess) `shouldThrow` anyException

    should "throw an error when executable can't be found when trying to writeLine" $ do
      childProcess <- spawn "kasdhfskhdg" [] "."
      threadDelay 10000
      (writeLine childProcess "foo") `shouldThrow` anyException

    should "throw an error twice when the executable can't be found and readLine is called twice" $ do
      childProcess <- spawn "kasdhfskhdg" [] "."
      (readLine childProcess) `shouldThrow` anyException
      (readLine childProcess) `shouldThrow` anyException

    should "throw an error twice when the executable can't be found and writeLine is called twice" $ do
      childProcess <- spawn "kasdhfskhdg" [] "."
      threadDelay 10000
      (writeLine childProcess "foo") `shouldThrow` anyException
      (writeLine childProcess "bar") `shouldThrow` anyException
