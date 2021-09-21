module EmptyGenServer where

import Prelude
import Books (Book, Isbn, BookEvent(..))
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.GenServer (CallResult(..))
import Pinto.GenServer as GenServer

type EmptyGenServerStartArgs
  = {}

type State
  = {}

serverName :: ServerName State Unit
serverName = Local $ atom "empty_gen_server"

startLink :: EmptyGenServerStartArgs -> Effect StartLinkResult
startLink args = GenServer.startLink serverName (init args)

init :: EmptyGenServerStartArgs -> GenServer.Init State Unit
init args = do
  pure $ {}
