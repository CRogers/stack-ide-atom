module Atom.CompositeDisposable (
  CompositeDisposable(..),
  newCompositeDisposable
) where

import GHCJS.Types

data JSCompositeDisposable_
type JSCompositeDisposable = JSRef JSCompositeDisposable_

data CompositeDisposable = CompositeDisposable {
  addDisposable :: JSRef a -> IO (),
  dispose :: IO ()
}

foreign import javascript unsafe
  "$1.add($2)" js_add :: JSCompositeDisposable -> JSRef a -> IO ()

foreign import javascript unsafe
  "$1.dispose()" js_dispose :: JSCompositeDisposable -> IO ()

fromJSCompositeDisposable :: JSCompositeDisposable -> CompositeDisposable
fromJSCompositeDisposable cd = CompositeDisposable {
  addDisposable = js_add cd,
  dispose = js_dispose cd
}

foreign import javascript unsafe
  "new require('atom').CompositeDisposable()" js_create :: IO JSCompositeDisposable

newCompositeDisposable :: IO CompositeDisposable
newCompositeDisposable = fromJSCompositeDisposable <$> js_create