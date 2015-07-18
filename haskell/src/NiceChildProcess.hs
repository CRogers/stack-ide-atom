{-# LANGUAGE OverloadedStrings #-}

module NiceChildProcess (
  CP.spawn
, readLine
) where

import GHCJS.Types

import qualified Node.ChildProcess as CP

readLine :: CP.ChildProcess -> IO JSString
readLine childProcess = return ""
