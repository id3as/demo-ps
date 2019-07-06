module BookSup where

import Effect
import Erl.Data.List
import Prelude

import BookWeb as BookWeb
import BookLibrary as BookLibrary
import BookConfig as BookConfig
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildShutdown(..), SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childRestart, childShutdown, childStart, childStartTemplate, childType, supervisorChildren, supervisorIntensity, supervisorPeriod, supervisorStrategy)
import Pinto.Sup as Sup

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "book_sup" init

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
                                       # childId "book_library"
                                       # childStart BookLibrary.startLink { connectionString } )
                                        : nil)
