module OneForOneGen where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process (self)
import Erl.Process.Raw (Pid)
import Foreign (unsafeToForeign)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (Action(..), InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Monitor as Monitor
import Pinto.Timer as Timer

foreign import getDataFromSomeNativeCode :: Effect Binary

type MessageHandler = (Binary -> Effect Unit)

type State =
  { clientPid :: Pid
  , handler :: MessageHandler
  , dataSent :: Int
  }

data Msg
  = ClientDisconnected
  | Tick

type OneForOneGenServerStartArgs =
  { clientPid :: Pid
  , handler :: MessageHandler
  }

serverName :: Pid -> RegistryName (ServerType Unit Unit Msg State)
serverName pid = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") $ (tuple2 "one_for_one_example" pid))

type OneForOneGenPid = ServerPid Unit Unit Msg State

startLink :: OneForOneGenServerStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec (init args)) { name = Just (serverName args.clientPid), handleInfo = Just handleInfo }

init :: OneForOneGenServerStartArgs -> GenServer.InitFn Unit Unit Msg State
init { clientPid, handler } = do
  self <- self
  void $ Timer.sendAfter (Milliseconds 500.0) Tick
  liftEffect do
    void $ Monitor.monitorTo clientPid self (\_ -> ClientDisconnected)
    pure $ InitOk { clientPid, handler, dataSent: 0 }

handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state@{ handler, dataSent } = do
  case msg of
    ClientDisconnected -> do
      pure $ GenServer.returnWithAction StopNormal state
    Tick -> do
      void $ Timer.sendAfter (Milliseconds 500.0) Tick
      liftEffect do
        void $ handler =<< getDataFromSomeNativeCode
        pure $ GenServer.return $ state { dataSent = dataSent + 1 }
