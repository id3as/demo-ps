module OneForOneSup where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (Pid)
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childRestart, childStartTemplate, childType, supervisorChildren, supervisorIntensity, supervisorPeriod, supervisorStrategy)
import Pinto.Sup as Sup
import Pinto.Types (ServerName(..))
import OneForOneGen as OneForOneGen

serverName :: SupervisorName
serverName = Local $ atom "one_for_one_sup"

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink args = 
  Sup.startLink serverName $ init args

init :: Unit -> Effect SupervisorSpec 
init _ = pure $ buildSupervisor
  # supervisorStrategy SimpleOneForOne 
  # supervisorIntensity 100
  # supervisorPeriod 60
  # supervisorChildren (( buildChild
                       # childType Worker
                       # childId "one_for_one_child"
                       # childRestart Transient
                       # childStartTemplate childTemplate
                       ) : nil)

childTemplate :: Pinto.ChildTemplate OneForOneGen.OneForOneGenStartArgs
childTemplate = Pinto.ChildTemplate (OneForOneGen.startLink)

startClient :: OneForOneGen.OneForOneGenStartArgs -> Effect Pid
startClient args = do
  result <- Sup.startSimpleChild childTemplate serverName args
  case result of
       Pinto.ChildAlreadyStarted pid -> pure pid
       Pinto.ChildStarted pid -> pure pid
       -- TODO ??
       something -> do
          throw "Failed to start child"
