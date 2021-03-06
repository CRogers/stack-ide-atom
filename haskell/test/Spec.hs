module Main where

import Test.Hspec

import qualified ChildProcessSpec as CPS
import qualified NiceChildProcessSpec as NCPS
import qualified StackIdeSpec as SIS
import qualified UpdateSkippingStackIdeProcessSpec as USSIPS
import qualified StackIdeProcessSpec as SIPS

main :: IO ()
main = hspec $ do
  CPS.spec
  NCPS.spec
  SIPS.spec
  USSIPS.spec
  SIS.spec