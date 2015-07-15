module Node.ChildProcess where

import Control.Monad ((=<<))

import GHCJS.Types
import GHCJS.Foreign

data ChildProcess_
type ChildProcess = JSRef ChildProcess_

type Command = JSString
type Arg = JSString
type Directory = JSString

foreign import javascript unsafe
  "require('child_process').spawn($1, $2, $3)"
  js_spawn :: JSString -> JSArray a -> JSObject b -> IO ChildProcess

spawn :: Command -> [Arg] -> Directory -> IO ChildProcess
spawn command args cwd = do
  options <- newObj
  setProp "cwd" cwd options

  js_args <- toArray args

  js_spawn command js_args options

data Stream_
type Stream = JSRef Stream_

foreign import javascript unsafe
  "$1.stdout" stdout :: ChildProcess -> IO Stream

foreign import javascript unsafe
  "$1.on($2, $3)" js_on :: Stream -> JSString -> JSFun (Buffer -> IO ()) -> IO ()

type EventName = JSString

data Buffer_
type Buffer = JSRef Buffer_

on :: Stream -> EventName -> (Buffer -> IO ()) -> IO ()
on stream eventName f = do
  callback <- syncCallback1 AlwaysRetain False f
  js_on stream eventName callback

foreign import javascript unsafe
  "$1.toString()" toString :: JSRef a -> IO JSString
