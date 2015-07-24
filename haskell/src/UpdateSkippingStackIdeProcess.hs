module UpdateSkippingStackIdeProcess (StackIdeProcess(..), createFromStackIdeProcess) where

import Stack.Ide.JsonAPI

import StackIdeProcess

createFromStackIdeProcess :: StackIdeProcess -> StackIdeProcess
createFromStackIdeProcess stackIdeProcess =
  StackIdeProcess (request stackIdeProcess) (makeAwaitResponse stackIdeProcess)

makeAwaitResponse :: StackIdeProcess -> IO Response
makeAwaitResponse stackIdeProcess = do
  response <- awaitResponse stackIdeProcess
  case response of
    ResponseUpdateSession _ -> makeAwaitResponse stackIdeProcess
    res -> return res