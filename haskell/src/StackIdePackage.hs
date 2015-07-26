module StackIdePackage where

import Atom.Package

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

onActivate :: IO ()
onActivate = do
  putStrLn "hi"