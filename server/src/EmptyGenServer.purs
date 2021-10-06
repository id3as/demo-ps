module EmptyGenServer where

import Prelude
import Erl.Atom (atom)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer

type EmptyGenServerStartArgs
  = {}

type State
  = {}

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = Local $ atom "empty_gen_server"

startLink :: EmptyGenServerStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

init :: EmptyGenServerStartArgs -> GenServer.InitFn Unit Unit Unit State
init _args = do
  pure $ InitOk {}
