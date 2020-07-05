module BookSup where

import Effect
import Erl.Data.List
import Prelude


import Erl.Atom (atom)
import BookWeb as BookWeb
import BookLibrary as BookLibrary
import BookConfig as BookConfig
import HandleInfoExample as HandleInfoExample
import MonitorExample as MonitorExample
import EmptyGenServer as EmptyGenServer
import OneForOneSup as OneForOneSup
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildShutdown(..), SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childRestart, childShutdown, childStart, childStartTemplate, childType, supervisorChildren, supervisorIntensity, supervisorPeriod, supervisorStrategy)
import Pinto.Sup as Sup

serverName :: Pinto.SupervisorName
serverName = (Pinto.Local $ atom "book_sup")

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink serverName init

init :: Effect SupervisorSpec
init = do
  connectionString <- BookConfig.connectionString
  webPort <- BookConfig.webPort
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                -- These are typed along with their start args, so nice
                -- They pretty much pair up exactly with the OTP docs
                # supervisorChildren ( ( buildChild
                                       # childType Worker
                                       # childId "book_web"
                                       # childStart BookWeb.startLink  { webPort } )
                                       : 
                                       ( buildChild
                                       # childType Worker
                                       # childId "empty_server"
                                       # childStart EmptyGenServer.startLink  {} )
                                       :
                                       ( buildChild
                                       # childType Worker
                                       # childId "book_library"
                                       # childStart BookLibrary.startLink { connectionString } )
                                       : 
                                       ( buildChild
                                       # childType Worker
                                       # childId "handle_info_example"
                                       # childStart HandleInfoExample.startLink {} )
                                       :
                                       ( buildChild
                                       # childType Worker
                                       # childId "monitor_example"
                                       # childStart MonitorExample.startLink {} )
                                       :
                                       ( buildChild
                                       # childType Supervisor
                                       # childId "one_for_one_example"
                                       # childStart OneForOneSup.startLink unit )
                                        : nil)
