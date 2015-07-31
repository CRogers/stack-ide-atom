{-# LANGUAGE OverloadedStrings #-}

module StackIdePackage where

import Atom.CommandRegistry
import Atom.Decoration
import Atom.Marker
import Atom.Package
import Atom.TextEditor
import Control.Exception
import Control.Monad
import Data.IORef
import qualified Data.Text as T
import Data.Traversable
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Utils
import IdeSession.Types.Public

import StackIde
import StackIdeM

foreign import javascript unsafe
  "atom.notifications.addError($1)" addError :: JSString -> IO ()

wrap :: IO () -> IO ()
wrap expr = catch expr handle
  where
    handle ex = addError (toJSString (show (ex :: SomeException)))

stackIdePackage :: Package
stackIdePackage = Package {
  activate = onActivate
}

foreign import javascript unsafe
  "$1.destroy()" js_destroy :: Marker -> IO ()

onActivate :: IO ()
onActivate = do
  markersRef <- newIORef []

  addCommand "atom-text-editor" "stack-ide-atom:source-errors" $ \editor -> wrap $ do
    path <- getPath editor
    let dir = T.dropWhileEnd (/= '/') path
    print dir
    sourceErrors <- runStackIde $ do
      createSession dir
      getSourceErrors
    print sourceErrors
    oldMarkers <- readIORef markersRef
    case oldMarkers of
      [] -> return ()
      [marker] -> js_destroy marker
    newMarkers <- flip traverse sourceErrors $ \sourceError -> do
      let (SourceError _ (ProperSpan (SourceSpan _ sx sy ex ey)) _) = sourceError
      let range = rangeBetween (sx - 1) (sy - 1) (ex - 1) (ey - 1)
      marker <- markBufferRange editor range
      consoleLog marker
      decoration <- decorateMarker editor marker "sia-error"
      consoleLog $ jsDecoration decoration
      return marker
    writeIORef markersRef newMarkers
  putStrLn "hi"