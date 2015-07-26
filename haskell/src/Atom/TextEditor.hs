module Atom.TextEditor where

import Control.Applicative
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Prim (toJSInt)
import GHCJS.Types

import Atom.Marker

data TextEditor_
type TextEditor = JSRef TextEditor_

foreign import javascript unsafe
  "$1.getPath()" js_getPath :: TextEditor -> IO JSString

type Path = Text

getPath :: TextEditor -> IO Path
getPath editor = fromJSString <$> js_getPath editor

data Range_
type Range = JSRef Range_

foreign import javascript unsafe
  "new require('atom').Range([$1, $2], [$3, $4]).freeze()"
  js_rangeBetween :: JSNumber -> JSNumber -> JSNumber -> JSNumber -> Range

rangeBetween :: Int -> Int -> Int -> Int -> Range
rangeBetween sx sy ex ey = do
  js_rangeBetween (toJSInt sx) (toJSInt sy) (toJSInt ex) (toJSInt ey)

foreign import javascript unsafe
  "$1.markBufferRange($2)" markBufferRange :: TextEditor -> Range -> IO Marker