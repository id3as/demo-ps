module NamelessGen where

import Prelude
import Books (Book, Isbn, BookEvent(..))
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Erl.Process.Raw (Pid)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen

type EmptyGenServerStartArgs
  = {}

type State
  = { value :: String }

setValue :: Pid -> String -> Effect Unit
setValue pid newValue = Gen.castByPid pid (\s -> pure $ CastNoReply s { value = newValue })

startLink :: EmptyGenServerStartArgs -> Effect StartLinkResult
startLink args = Gen.startLinkNameless (init args)

init :: EmptyGenServerStartArgs -> Gen.Init State Unit
init args = do
  pure $ { value: "" }
