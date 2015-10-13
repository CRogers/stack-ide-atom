{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module StackIdeSpec where

import Test.Hspec

import Control.Monad
import IdeSession.Types.Public

import StackIdeM
import StackIde

should = it
xshould _ _ = return ()

spec :: Spec
spec =
  describe "StackIde should" $ do
    should "respond with the version when starting up" $ do
      let program = do
                    createSession "test-data/no-compile-errors"
                    getVersion
      versionInfo <- runStackIde program
      versionInfo `shouldBe` VersionInfo 0 1 1

    should "respond with a source error if a file has a source error" $ do
      let program = do
                    createSession "test-data/one-source-error"
                    getSourceErrors
      sourceErrors <- runStackIde program
      length sourceErrors `shouldBe` 1
      let sourceError = head sourceErrors
      errorSpan sourceError `shouldBe` ProperSpan (SourceSpan "OneSourceError.hs" 4 8 4 9)