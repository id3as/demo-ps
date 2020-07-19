module CounterExample where

import Prelude

import Erl.Atom (atom)
import Effect (Effect)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen

type CounterExampleStartArgs = { initialValue :: Int }
type State = { value :: Int }

serverName :: ServerName State Unit
serverName = Local $ atom "counter_example"

startLink :: CounterExampleStartArgs -> Effect StartLinkResult
startLink args =
   Gen.startLink serverName (init args)

init :: CounterExampleStartArgs -> Gen.Init State  Unit
init args = do
  pure $ { value: args.initialValue}

current :: Effect Int
current = 
  Gen.call serverName (\s -> pure $ Gen.CallReply s.value s)

add :: Int -> Effect Unit
add a = 
  Gen.cast serverName (\s@{ value  } -> pure $ Gen.CastNoReply s { value = value + a } )
