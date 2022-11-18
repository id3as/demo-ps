module ProcessSpawnLink where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Process (Process, ProcessM, receiveWithTimeout, self, (!))
import Erl.Process as Process
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer

data ChildMsg
  = Add Int
  | Subtract Int
  | Timeout
  | Finish

type ProcessSpawnLinkStartArgs = {}
type State =
  { child :: Process ChildMsg
  }

data Msg = Tick

serverName :: RegistryName (ServerType Unit Unit Msg State)
serverName = Local $ atom "process_spawn_link"

startLink :: ProcessSpawnLinkStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec (init args)) { name = Just serverName, handleInfo = Just handleInfo }

init :: ProcessSpawnLinkStartArgs -> GenServer.InitFn Unit Unit Msg State
init _args = do
  self <- self
  child <- liftEffect $ Process.spawnLink $ childLoop self 0
  pure $ InitOk { child }

handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state@{ child } =
  case msg of
    Tick -> do
      liftEffect $ child ! (Add 1)
      pure $ GenServer.return state

childLoop :: Process Msg -> Int -> ProcessM ChildMsg Unit
childLoop parent value = do
  msg <- receiveWithTimeout (Milliseconds 1000.0) Timeout
  case msg of
    Add x ->
      childLoop parent $ value + x

    Subtract x ->
      childLoop parent $ value - x

    Timeout -> do
      liftEffect $ parent ! Tick
      childLoop parent value
    _ -> do
      pure unit

