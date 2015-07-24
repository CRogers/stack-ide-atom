module UpdateSkippingStackIdeProcess (StackIdeProcess(..), createFromStackIdeProcess) where

import Stack.Ide.JsonAPI

import StackIdeProcess

createFromStackIdeProcess :: StackIdeProcess -> StackIdeProcess
createFromStackIdeProcess stackIdeProcess =
  StackIdeProcess (makeRequest stackIdeProcess) (makeAwaitResponse stackIdeProcess)

makeRequest :: StackIdeProcess -> Request -> IO ()
makeRequest = request

makeAwaitResponse :: StackIdeProcess -> IO Response
makeAwaitResponse = awaitResponse