module Atom.CommandRegistry where

import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Types

data Event_
type Event = JSRef Event_

foreign import javascript unsafe
  "atom.commands.add($1, $2, $3)" js_addCommand :: JSString -> JSString -> JSFun (Event -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1.target.getModel().getPath()" js_getPath :: Event -> IO JSString

type Target = Text
type CommandName = Text

addCommand :: Target -> CommandName -> (Text -> IO ()) -> IO ()
addCommand target commandName action = do
  let wrappedAction event = do
                              path <- js_getPath event
                              action (fromJSString path)
  callback <- asyncCallback1 AlwaysRetain wrappedAction
  js_addCommand (toJSString target) (toJSString commandName) callback