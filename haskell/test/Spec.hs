module Main where

import Test.Hspec

import qualified ChildProcessSpec as CPS
import qualified NiceChildProcessSpec as NCPS
import qualified StackIdeSpec as SIS

main :: IO ()
main = hspec $ do
  CPS.spec
  NCPS.spec
  SIS.spec
