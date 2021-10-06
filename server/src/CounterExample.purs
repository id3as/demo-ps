module CounterExample where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto (StartLinkResult, RegistryName(..), RegistryReference(..))
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer

type CounterExampleStartArgs
  = { initialValue :: Int }

type State
  = { value :: Int }

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = Local $ atom "counter_example"

startLink :: CounterExampleStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

init :: CounterExampleStartArgs -> GenServer.InitFn Unit Unit Unit State
init args = do
  pure $ InitOk { value: args.initialValue }

current :: Effect Int
current = GenServer.call (ByName serverName) (\_f s -> pure $ GenServer.reply s.value s)

add :: Int -> Effect Unit
add a = GenServer.cast (ByName serverName) (\s@{ value } -> pure $ GenServer.return s { value = value + a })
