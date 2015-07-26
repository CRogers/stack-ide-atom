module Atom.CommandRegistry where

import Control.Monad
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Types

import Atom.TextEditor (TextEditor)

data Event_
type Event = JSRef Event_

foreign import javascript unsafe
  "atom.commands.add($1, $2, $3)" js_addCommand :: JSString -> JSString -> JSFun (Event -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1.target.getModel()" js_getTextEditor :: Event -> IO TextEditor

type Target = Text
type CommandName = Text

addCommand :: Target -> CommandName -> (TextEditor -> IO ()) -> IO ()
addCommand target commandName action = do
  let wrappedAction event = action =<< js_getTextEditor event
  callback <- asyncCallback1 AlwaysRetain wrappedAction
  js_addCommand (toJSString target) (toJSString commandName) callback