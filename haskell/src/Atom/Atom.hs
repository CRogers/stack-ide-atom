module Atom.Atom where

import GHCJS.Types

data Atom_
type Atom = JSRef Atom_

foreign import javascript unsafe
  "atom" :: Atom