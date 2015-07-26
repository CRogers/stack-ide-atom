{-# LANGUAGE OverloadedStrings #-}

module StackIdePackage where

import Atom.CommandRegistry
import Atom.Package
import Control.Monad
import GHCJS.Utils

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

onActivate :: IO ()
onActivate = do
  addCommand "atom-text-editor" "stack-ide-atom:source-errors" $ \path ->
    putStrLn $ show path
  putStrLn "hi"