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
import Pinto.Gen (CallResult(..), CastResult(..), defaultStartLink)
import Pinto.Gen as Gen

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
    Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: ProcessSpawnLinkStartArgs -> Gen.Init State Msg
init args = do
  self <- Gen.self
  child <- Gen.lift $ Process.spawnLink $ childLoop self 0
  pure $ { child }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ child } = 
  case msg of
     Tick -> do
       Gen.lift $ child ! (Add 1)
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

