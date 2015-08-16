{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module StackIdeProcess (StackIdeProcess(..), createStackIdeProcess, createFromChildProcess, command) where

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

command :: StackIdeProcess -> Request -> IO Response
command StackIdeProcess{..} req = do
  request req
  awaitResponse

createStackIdeProcess :: Text -> IO StackIdeProcess
createStackIdeProcess directory = do
  createFromChildProcess <$> spawn "stack" ["ide", "start"] directory

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