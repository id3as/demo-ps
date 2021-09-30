module OneForOneSup where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import OneForOneGen (OneForOneGenPid, OneForOneGenServerStartArgs)
import OneForOneGen as OneForOneGen
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), crashIfChildNotStarted)
import Pinto.Supervisor.SimpleOneForOne (ChildSpec)
import Pinto.Supervisor.SimpleOneForOne as Sup

serverName :: RegistryName (Sup.SupervisorType OneForOneGenServerStartArgs OneForOneGenPid)
serverName = Local $ atom $ "one_for_one_example"

startLink :: Effect (StartLinkResult (Sup.SupervisorPid OneForOneGenServerStartArgs OneForOneGenPid))
startLink = Sup.startLink (Just $ Local $ atom "running_game_sup") init

init :: Effect (ChildSpec OneForOneGenServerStartArgs OneForOneGenPid)
init =
  pure { intensity: 100
    , period: Seconds 60.0
    , childType: Worker
    , start: OneForOneGen.startLink
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
  } 

startClient :: OneForOneGenServerStartArgs -> Effect OneForOneGenPid
startClient args = do
  crashIfChildNotStarted <$> Sup.startChild (ByName serverName) args

