module Atom.Package where

import Control.Applicative
import GHCJS.Types
import GHCJS.Foreign

data Package = Package {
  activate :: IO ()
}

data JSPackage_
type JSPackage = JSRef JSPackage_

runPackage :: Package -> IO JSPackage
runPackage package = do
  obj <- newObj
  f <- syncCallback AlwaysRetain False (activate package)
  setProp "activate" f obj
  return obj