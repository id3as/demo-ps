module BookSup where

import Prelude

import BookConfig as BookConfig
import BookLibrary as BookLibrary
import BookWeb as BookWeb
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import EmptyGenServer as EmptyGenServer
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (class HasPid)
import HandleInfoExample as HandleInfoExample
import MonitorExample as MonitorExample
import OneForOneSup as OneForOneSup
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), ErlChildSpec, RestartStrategy(..), Strategy(..), SupervisorPid, SupervisorSpec, spec)
import Pinto.Supervisor as Sup

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = do
  Sup.startLink (Just $ Local $ atom "example_usp") init

init :: Effect SupervisorSpec
init = do
  connectionString <- BookConfig.connectionString
  webPort <- BookConfig.webPort
  -- Unsurprisingly, this looks just like the specs in the Erlang docs
  -- this is intentional
  -- the difference being that the args for the startLinks are passed in to create Effect Units 
  -- which means they're typed
  pure
    { flags:
        { strategy: OneForOne
        , intensity: 1
        , period: Seconds 5.0
        }
    , childSpecs:
        (worker "book_web" $ BookWeb.startLink { webPort })
        : (worker "empty_server" $ EmptyGenServer.startLink {})
        : (worker "book_library" $ BookLibrary.startLink { connectionString })
        : (worker "handle_info_example" $ HandleInfoExample.startLink {})
        : (worker "monitor_example" $ MonitorExample.startLink {})
        : (worker "one_for_one_example" $ OneForOneSup.startLink)
        : nil
    }

-- A convenience function to set up a child with defaults
-- Note: There is no rule here saying that a child process should be a GenServer or a GenStateM, it just needs 
-- to be something that has a pid (and for erlang itself, should probably use proc_lib..)
worker ::
  forall childProcess.
  HasPid childProcess =>
  String -> Effect (StartLinkResult childProcess) -> ErlChildSpec
worker id start =
  spec
    { id
    , childType: Worker
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
    }
