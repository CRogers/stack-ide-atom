module Atom.CommandRegistry where

import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe
  "atom.commands.add($1, $2, $3)" js_addCommand :: JSString -> JSString -> JSFun (IO ()) -> IO ()

type Target = Text
type CommandName = Text

addCommand :: Target -> CommandName -> IO () -> IO ()
addCommand target commandName action = do
  callback <- syncCallback AlwaysRetain False action
  js_addCommand (toJSString target) (toJSString commandName) callback