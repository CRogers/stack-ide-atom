{-# LANGUAGE OverloadedStrings #-}

module StackIdeProcess (StackIdeProcess(..), createStackProcess, createFromChildProcess) where

import Control.Applicative
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Stack.Ide.JsonAPI (Request, Response)

import NiceChildProcess

data StackIdeProcess = StackIdeProcess {
  request :: Request -> IO (),
  awaitResponse :: IO Response
}

createStackProcess :: Text -> IO StackIdeProcess
createStackProcess directory = do
  createFromChildProcess <$> spawn "stack" ["ide"] directory

createFromChildProcess :: ChildProcess -> StackIdeProcess
createFromChildProcess childProcess =
  StackIdeProcess (makeRequest childProcess) (makeAwaitResponse childProcess)

makeRequest :: ChildProcess -> Request -> IO ()
makeRequest childProcess req =
  writeLine childProcess (decodeUtf8 (toStrict (encode req)))

makeAwaitResponse :: ChildProcess -> IO Response
makeAwaitResponse childProcess = do
  responseStr <- readLine childProcess
  let (Just response) = decode (fromStrict $ encodeUtf8 responseStr)
  return response