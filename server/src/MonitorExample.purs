module MonitorExample where

import Prelude

import Books (Book,  Isbn, BookEvent(..))
import Erl.Process.Raw (Pid)
import Erl.Atom (atom)
import Data.Either (Either(..))
import Erl.Data.Binary (Binary(..))
import Data.Maybe (Maybe(..))
import Erl.Data.Map as Map
import Data.Traversable (traverse)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult, self)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Pinto.Monitor as Monitor
import Books (Book, Isbn)
import Redis (ConnectionString, DbId, RedisConnection)
import Redis as Redis
import SimpleBus as SimpleBus
import BookLibrary as BookLibrary
import Logger as Logger

type MessageHandler = (Binary -> Effect Unit)
type BookWatchingStartArgs = {}

foreign import getDataFromSomeNativeCode :: Effect Binary


type State = {
  handlers :: Map.Map Pid MessageHandler
}

data Msg = ClientDisconnected Pid
         | Tick
  

serverName :: ServerName State Msg
serverName = Local $ atom "monitor_example"

startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }


registerClient :: MessageHandler -> Effect Unit
registerClient handler = do
  handlerPid <- self
  Gen.doCall serverName \state -> do
     newState <- addHandler handler handlerPid  state
     pure $ CallReply unit newState

init :: BookWatchingStartArgs -> Effect State
init args = do
  void $ Timer.sendAfter 500 Tick =<< Gen.emitter serverName
  pure $ {
    handlers: Map.empty
  }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{ handlers  } = do
  case msg of
     ClientDisconnected handlerPid -> do
        void $ Logger.info1 "Removing ~p as it disconnected" handlerPid
        pure $ CastNoReply $ state { handlers = Map.delete handlerPid handlers }
     Tick -> do
        sendData handlers
        void $ Timer.sendAfter 500 Tick =<< Gen.emitter serverName
        pure $ CastNoReply $ state 

addHandler :: MessageHandler -> Pid -> State -> Effect State
addHandler handler handlerPid state@{ handlers } = do
  emitter <- Gen.emitter serverName
  void $ Logger.info1 "Adding handler ~p as it has connected" handlerPid
  void $ Monitor.monitor handlerPid (\_ -> emitter $ ClientDisconnected handlerPid)
  pure $ state { handlers = Map.insert handlerPid handler handlers }


sendData :: Map.Map Pid MessageHandler -> Effect Unit
sendData handlers = do
  freshData <- getDataFromSomeNativeCode
  void $ traverse (\handler -> do handler freshData) $ Map.values handlers 
  pure unit
