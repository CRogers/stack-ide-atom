{-# LANGUAGE NoMonomorphismRestriction #-}

module StackIdeSpec where

import Test.Hspec

import Control.Monad

import StackIdeM
import StackIde

should = it
xshould _ _ = return ()

spec :: Spec
spec =
  describe "StackIde should" $ do
    should "respond with the version when starting up" $ do
      let program = do
                    createSession "test-data/just-stack-yaml"
                    getVersion
      versionInfo <- runStackIde program
      versionInfo `shouldBe` VersionInfo 0 1 0

    should "respond with a source error if a file has a source error" $ do
      let program = do
                    createSession "test-data/one-source-error"
                    getSourceErrors
      sourceErrors <- runStackIde program
      length sourceErrors `shouldBe` 1