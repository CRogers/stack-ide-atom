module StackIdeSpec where

import Test.Hspec

import Control.Monad

import StackIdeM
import StackIde

should = it

spec :: Spec
spec =
  describe "StackIde should" $ do
    should "respond with the version when starting up" $ do
      let program = do
                    createSession "test-data/just-stack-yaml"
                    getVersion
      void (runStackIde program :: IO VersionInfo)