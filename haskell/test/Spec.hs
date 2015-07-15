module Main where

import Test.Hspec

import qualified ChildProcessSpec as CPS

main :: IO ()
main = hspec $ do
  CPS.spec
