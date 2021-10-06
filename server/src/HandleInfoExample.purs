module HandleInfoExample where

import Prelude

import Books (BookEvent(..), Isbn)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom, Atom)
import Erl.Data.List (List, (:), nil)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ReturnResult, ServerPid, ServerType)
import Pinto.GenServer as GenServer

type BookWatchingStartArgs
  = {}

type State
  = {}

data Msg
  = BookMsg BookEvent

serverName :: RegistryName (ServerType Unit Unit Msg State)
serverName = Local $ atom "handle_info_example"

startLink :: BookWatchingStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec (init args)) { name = Just serverName, handleInfo = Just handleInfo }

init :: BookWatchingStartArgs -> GenServer.InitFn Unit Unit Msg State 
init _args = do
  -- We can get hold of 'self', which is of type (Process Msg) as defined by serverName
  -- Anything sent to this will appear in our handleInfo (ooh typing)
 -- self <- self

  -- SimpleBus.subscribe takes a bus
  -- and an emitter (msg -> Effect Unit)
  -- we can lift msg into the right type and send to self
--  _ <- GenServer.liftEffect $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self
  pure $ InitOk {}

handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state = 
  case msg of
    -- And that means we can switch over the messages as they come in
    -- All the typing..
   BookMsg bookEvent -> liftEffect $ handleBookEvent bookEvent state

-- And all we will do is log that we saw the events
-- as this demo ends here
handleBookEvent :: BookEvent -> State -> Effect (ReturnResult Unit Unit State)
handleBookEvent ev state = unsafeCrashWith "Not mplemented" 
  case ev of
    BookCreated isbn -> do
      _ <- logInfo "Book created" isbn
      pure $ GenServer.return state
    BookDeleted isbn -> do
      _ <- logInfo "Book deleted" isbn
      pure $ GenServer.return state
    BookUpdated isbn -> do
      _ <- logInfo "Book updated" isbn
      pure $ GenServer.return state

domain :: List Atom
domain = (atom "demo_ps") : (atom "handle_info_example") : nil

logInfo :: String -> Isbn -> Effect Unit
logInfo text isbn = Logger.info { domain, text, isbn, type: Logger.Trace } {}
