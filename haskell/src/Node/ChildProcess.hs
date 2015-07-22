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
  "$1.stdin" stdin :: ChildProcess -> IO Stream

foreign import javascript unsafe
  "$1.write($2)" write :: Stream -> JSString -> IO ()

foreign import javascript unsafe
  "$1.on($2, $3)" js_on :: JSRef a -> JSString -> JSFun (JSRef b -> IO ()) -> IO ()

type EventName = JSString

data Buffer_
type Buffer = JSRef Buffer_

on :: Stream -> EventName -> (Buffer -> IO ()) -> IO ()
on stream eventName f = do
  callback <- syncCallback1 AlwaysRetain False f
  js_on stream eventName callback

onData :: Stream -> (Buffer -> IO ()) -> IO ()
onData = (`on` toJSString "data")

data Err_
type Err = JSRef Err_

foreign import javascript unsafe
  "$1.message" errorMessage :: Err -> JSString

onError :: ChildProcess -> (Err -> IO ()) -> IO ()
onError childProcess f = do
  callback <- syncCallback1 AlwaysRetain False f
  childProcess `js_on` (toJSString "error") $ callback

foreign import javascript unsafe
  "$1.toString()" toString :: JSRef a -> IO JSString
