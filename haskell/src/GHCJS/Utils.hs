module GHCJS.Utils where

import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe
  "console.log($1)" consoleLog :: JSRef a -> IO ()

foreign import javascript unsafe
  "(function() { return this; })()" this :: IO (JSRef a) 