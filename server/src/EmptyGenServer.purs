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
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen

type EmptyGenServerStartArgs
  = {}

type State
  = {}

serverName :: ServerName State Unit
serverName = Local $ atom "empty_gen_server"

startLink :: EmptyGenServerStartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args)

init :: EmptyGenServerStartArgs -> Gen.Init State Unit
init args = do
  pure $ {}
