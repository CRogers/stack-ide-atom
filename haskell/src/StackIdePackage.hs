{-# LANGUAGE OverloadedStrings #-}

module StackIdePackage where

import Atom.CommandRegistry
import Atom.Decoration
import Atom.Marker
import Atom.Package
import Atom.TextEditor
import Control.Monad
import Data.IORef
import qualified Data.Text as T
import GHCJS.Utils
import IdeSession.Types.Public

import StackIde
import StackIdeM

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

foreign import javascript unsafe
  "$1.destroy()" js_destroy :: Marker -> IO ()

onActivate :: IO ()
onActivate = do
  markerRef <- newIORef Nothing

  addCommand "atom-text-editor" "stack-ide-atom:source-errors" $ \editor -> do
    path <- getPath editor
    let dir = T.dropWhileEnd (/= '/') path
    print dir
    sourceErrors <- runStackIde $ do
      createSession dir
      getSourceErrors
    print sourceErrors
    oldMarker <- readIORef markerRef
    case oldMarker of
      Nothing -> return ()
      Just marker -> js_destroy marker
    case length sourceErrors of
      0 -> return ()
      _ -> do
        let (SourceError _ (ProperSpan (SourceSpan _ sx sy ex ey)) _) = head sourceErrors
        let range = rangeBetween (sx - 1) (sy - 1) (ex - 1) (ey - 1)
        marker <- markBufferRange editor range
        writeIORef markerRef $ Just marker
        consoleLog marker
        decoration <- decorateMarker editor marker "sia-error"
        consoleLog $ jsDecoration decoration
  putStrLn "hi"