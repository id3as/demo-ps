module OneForOneSup where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import OneForOneGen (OneForOneGenPid, OneForOneGenServerStartArgs)
import OneForOneGen as OneForOneGen
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), crashIfChildNotStarted)
import Pinto.Sup.Dynamic (DynamicSpec)
import Pinto.Sup.Dynamic as Sup

serverName :: RegistryName (Sup.DynamicType OneForOneGenServerStartArgs OneForOneGenPid)
serverName = Local $ atom $ "one_for_one_example"

startLink :: Effect (StartLinkResult (Sup.DynamicPid OneForOneGenServerStartArgs OneForOneGenPid))
startLink = Sup.startLink (Just $ Local $ atom "running_game_sup") init

init :: Effect (DynamicSpec OneForOneGenServerStartArgs OneForOneGenPid)
init =
  pure { intensity: 100
    , period: 60
    , childType: Worker
    , start: OneForOneGen.startLink
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout 5000
  } 

startClient :: OneForOneGenServerStartArgs -> Effect OneForOneGenPid
startClient args = do
  crashIfChildNotStarted <$> Sup.startChild (ByName serverName) args

