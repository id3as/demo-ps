module MonitorExample where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom, Atom)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List, (:), nil)
import Erl.Data.Map as Map
import Erl.Process (Process, self)
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Logger as Logger
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Monitor as Monitor
import Pinto.Timer as Timer

foreign import getDataFromSomeNativeCode :: Effect Binary

type State =
  { handlers :: Map.Map Pid MessageHandler
  }

type MessageHandler = (Binary -> Effect Unit)

data Msg
  = ClientDisconnected Pid
  | Tick

type BookWatchingStartArgs = {}

serverName :: RegistryName (ServerType Unit Unit Msg State)
serverName = Local $ atom "monitor_example"

startLink :: BookWatchingStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec (init args)) { name = Just serverName, handleInfo = Just handleInfo }

init :: BookWatchingStartArgs -> GenServer.InitFn Unit Unit Msg State
init _args = do
  void $ Timer.sendAfter (Milliseconds 500.0) Tick
  pure
    $ InitOk { handlers: Map.empty }

registerClient :: MessageHandler -> Effect Unit
registerClient handler = do
  handlerPid <- Raw.self
  GenServer.call (ByName serverName) \_from state -> do
    self <- self
    newState <- liftEffect $ addHandler handler self handlerPid state
    pure $ GenServer.reply unit newState

handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state@{ handlers } = do
  case msg of
    ClientDisconnected handlerPid -> do
      void $ liftEffect $ logInfo "Removing as it disconnected" handlerPid
      pure $ GenServer.return state { handlers = Map.delete handlerPid handlers }
    Tick -> do
      liftEffect $ sendData handlers
      void $ Timer.sendAfter (Milliseconds 500.0) Tick
      pure $ GenServer.return state

addHandler :: MessageHandler -> Process Msg -> Pid -> State -> Effect State
addHandler handler self handlerPid state@{ handlers } = do
  void $ logInfo "Adding handler as it has connected" handlerPid
  void $ Monitor.monitorTo handlerPid self (const $ ClientDisconnected handlerPid)
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
