{-# LANGUAGE OverloadedStrings #-}

module StackIdePackage where

import Atom.CommandRegistry
import Atom.Package

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

onActivate :: IO ()
onActivate = do
  addCommand "atom-text-editor" "stack-ide-atom:source-errors" (putStrLn "command")
  putStrLn "hi"