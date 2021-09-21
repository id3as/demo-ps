module MonitorExample where

import Prelude
import Books (Book, Isbn, BookEvent(..))
import Erl.Process.Raw (Pid)
import Erl.Process ((!))
import Erl.Atom (atom, Atom)
import Data.Either (Either(..))
import Erl.Data.Binary (Binary(..))
import Data.Maybe (Maybe(..))
import Erl.Data.Map as Map
import Erl.Process (Process)
import Data.Traversable (traverse)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List, (:), nil)
import Pinto as Pinto
import Pinto (ServerName(..), StartLinkResult)
import Pinto.GenServer (CallResult(..), CastResult(..))
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import Pinto.Monitor as Monitor
import Books (Book, Isbn)
import Redis (ConnectionString, DbId, RedisConnection)
import Redis as Redis
import SimpleBus as SimpleBus
import BookLibrary as BookLibrary
import Logger as Logger

foreign import getDataFromSomeNativeCode :: Effect Binary

type State
  = { handlers :: Map.Map Pid MessageHandler
    }

type MessageHandler
  = (Binary -> Effect Unit)

data Msg
  = ClientDisconnected Pid
  | Tick

type BookWatchingStartArgs
  = {}

serverName :: ServerName State Msg
serverName = Local $ atom "monitor_example"

startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  GenServer.buildStartLink serverName (init args)
    $ GenServer.defaultStartLink { handleInfo = handleInfo }

init :: BookWatchingStartArgs -> GenServer.Init State Msg
init args = do
  self <- GenServer.self
  void $ GenServer.lift $ Timer.sendAfter 500 Tick self
  pure
    $ { handlers: Map.empty
      }

registerClient :: MessageHandler -> Effect Unit
registerClient handler = do
  handlerPid <- Pinto.self
  GenServer.doCall serverName \state -> do
    self <- GenServer.self
    newState <- GenServer.lift $ addHandler handler self handlerPid state
    pure $ CallReply unit newState

handleInfo :: Msg -> State -> GenServer.HandleInfo State Msg
handleInfo msg state@{ handlers } = do
  case msg of
    ClientDisconnected handlerPid -> do
      void $ GenServer.lift $ logInfo "Removing as it disconnected" handlerPid
      pure $ CastNoReply $ state { handlers = Map.delete handlerPid handlers }
    Tick -> do
      GenServer.lift $ sendData handlers
      self <- GenServer.self
      void $ GenServer.lift $ Timer.sendAfter 500 Tick self
      pure $ CastNoReply $ state

addHandler :: MessageHandler -> Process Msg -> Pid -> State -> Effect State
addHandler handler self handlerPid state@{ handlers } = do
  void $ logInfo "Adding handler as it has connected" handlerPid
  void $ Monitor.pid handlerPid (\_ -> self ! ClientDisconnected handlerPid)
  pure $ state { handlers = Map.insert handlerPid handler handlers }

sendData :: Map.Map Pid MessageHandler -> Effect Unit
sendData handlers = do
  freshData <- getDataFromSomeNativeCode
  void $ traverse (\handler -> do handler freshData) $ Map.values handlers
  pure unit

domain :: List Atom
domain = (atom "demo_ps") : (atom "monitor_example") : nil

logInfo :: String -> Pid -> Effect Unit
logInfo text pid = Logger.info { domain, text, pid, type: Logger.Trace } {}
