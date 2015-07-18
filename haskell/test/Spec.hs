module Main where

import Test.Hspec

import qualified ChildProcessSpec as CPS
import qualified NiceChildProcessSpec as NCPS

main :: IO ()
main = hspec $ do
  CPS.spec
  NCPS.spec
