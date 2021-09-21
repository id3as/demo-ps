module ProcessSpawnLink where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List)
import Erl.Process (Process(..), SpawnedProcessState, (!))
import Erl.Process as Process
import Erl.Process.Raw (receiveWithTimeout)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.GenServer (CallResult(..), CastResult(..), defaultStartLink)
import Pinto.GenServer as GenServer

data ChildMsg = Add Int
              | Subtract Int
              | Timeout

type ProcessSpawnLinkStartArgs = {}
type State = {
  child :: Process ChildMsg
  }


data Msg = Tick

serverName :: ServerName State Msg
serverName = Local $ atom "empty_gen_server"

startLink :: ProcessSpawnLinkStartArgs -> Effect StartLinkResult
startLink args =
    GenServer.buildStartLink serverName (init args) $ GenServer.defaultStartLink { handleInfo = handleInfo }

init :: ProcessSpawnLinkStartArgs -> GenServer.Init State Msg
init args = do
  self <- GenServer.self
  child <- GenServer.lift $ Process.spawnLink $ childLoop self 0
  pure $ { child }

handleInfo :: Msg -> State -> GenServer.HandleInfo State Msg
handleInfo msg state@{ child } = 
  case msg of
     Tick -> do
       GenServer.lift $ child ! (Add 1)
       pure $ CastNoReply state


childLoop :: Process Msg -> Int -> SpawnedProcessState ChildMsg -> Effect Unit
childLoop parent value s@{ receiveWithTimeout } = do
  msg <- receiveWithTimeout 5000 Timeout
  newValue <- case msg of
                Add x -> 
                  pure (value - x)

                Subtract x  ->
                  pure (value - x)

                Timeout -> do
                  parent ! Tick
                  pure value
  childLoop parent newValue s

