module Atom.Decoration where

import GHCJS.Types

import Atom.Marker

data JSDecoration_
type JSDecoration = JSRef JSDecoration_

data Decoration = Decoration {
  jsDecoration :: JSDecoration,
  marker :: Marker
}

foreign import javascript unsafe
  "$1.getMarker()" js_getMarker :: JSDecoration -> Marker

fromJSDecoration :: JSDecoration -> Decoration
fromJSDecoration decoration = Decoration {
  jsDecoration = decoration,
  marker = js_getMarker decoration
}