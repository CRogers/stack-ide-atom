{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHCJS.Foreign
import GHCJS.Types

import Atom.Package
import StackIdePackage

main :: IO ()
main = return ()

foreign import javascript unsafe
  "$1($2)" invokeCallback :: JSFun (JSRef a -> IO ()) -> JSRef a -> IO ()

foreign import javascript unsafe
  "startDebugger" debugger :: IO ()

getPackage :: JSFun (JSPackage -> IO ()) -> IO ()
getPackage callback = do
  package <- runPackage stackIdePackage
  invokeCallback callback package