module CounterExample where

import Prelude
import Erl.Atom (atom)
import Effect (Effect)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.GenServer as GenServer

type CounterExampleStartArgs
  = { initialValue :: Int }

type State
  = { value :: Int }

serverName :: ServerName State Unit
serverName = Local $ atom "counter_example"

startLink :: CounterExampleStartArgs -> Effect StartLinkResult
startLink args = GenServer.startLink serverName (init args)

init :: CounterExampleStartArgs -> GenServer.Init State Unit
init args = do
  pure $ { value: args.initialValue }

current :: Effect Int
current = GenServer.doCall serverName (\s -> pure $ GenServer.CallReply s.value s)

add :: Int -> Effect Unit
add a = GenServer.doCast serverName (\s@{ value } -> pure $ GenServer.CastNoReply s { value = value + a })
