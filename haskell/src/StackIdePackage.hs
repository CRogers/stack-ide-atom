{-# LANGUAGE OverloadedStrings #-}

module StackIdePackage where

import Atom.CommandRegistry
import Atom.Package
import Atom.TextEditor
import Control.Monad
import qualified Data.Text as T
import GHCJS.Utils
import IdeSession.Types.Public

import StackIde
import StackIdeM

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

onActivate :: IO ()
onActivate = do
  addCommand "atom-text-editor" "stack-ide-atom:source-errors" $ \editor -> do
    path <- getPath editor
    let dir = T.dropWhileEnd (/= '/') path
    print dir
    sourceErrors <- runStackIde $ do
      createSession dir
      getSourceErrors
    print sourceErrors
    let (SourceError _ (ProperSpan (SourceSpan _ sx sy ex ey)) _) = head sourceErrors
    let range = rangeBetween sx sy ex ey
    marker <- markBufferRange editor range
    consoleLog marker
  putStrLn "hi"