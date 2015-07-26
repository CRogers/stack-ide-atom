{-# LANGUAGE OverloadedStrings #-}

module StackIdePackage where

import Atom.CommandRegistry
import Atom.Package
import Control.Monad
import qualified Data.Text as T
import GHCJS.Utils

import StackIde
import StackIdeM

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

onActivate :: IO ()
onActivate = do
  addCommand "atom-text-editor" "stack-ide-atom:source-errors" $ \path -> do
    let dir = T.dropWhileEnd (/= '/') path
    print dir
    sourceErrors <- runStackIde $ do
      createSession dir
      getSourceErrors
    print sourceErrors
  putStrLn "hi"