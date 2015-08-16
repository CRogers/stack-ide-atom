module UpdateSkippingStackIdeProcess (StackIdeProcess(..), createUpdateSkippingStackIdeProcess, createFromStackIdeProcess) where

import Control.Applicative
import Data.Text (Text)
import Stack.Ide.JsonAPI

import StackIdeProcess

createUpdateSkippingStackIdeProcess :: Text -> IO StackIdeProcess
createUpdateSkippingStackIdeProcess directory =
  createFromStackIdeProcess <$> createStackIdeProcess directory

createFromStackIdeProcess :: StackIdeProcess -> StackIdeProcess
createFromStackIdeProcess stackIdeProcess =
  StackIdeProcess (request stackIdeProcess) (makeAwaitResponse stackIdeProcess)

makeAwaitResponse :: StackIdeProcess -> IO Response
makeAwaitResponse stackIdeProcess = do
  response <- awaitResponse stackIdeProcess
  case response of
    ResponseUpdateSession _ -> makeAwaitResponse stackIdeProcess
    ResponseLog _ -> makeAwaitResponse stackIdeProcess
    res -> return res