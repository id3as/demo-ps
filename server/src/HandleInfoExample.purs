module HandleInfoExample where

import Prelude
import Books (Book, Isbn, BookEvent(..))
import Erl.Atom (atom, Atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List, (:), nil)
import Pinto (ServerName(..), StartLinkResult)
import Erl.Process ((!), send)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Books (Book, Isbn)
import Redis (ConnectionString, DbId, RedisConnection)
import Redis as Redis
import SimpleBus as SimpleBus
import BookLibrary as BookLibrary
import Logger as Logger

type BookWatchingStartArgs
  = {}

type State
  = {}

data Msg
  = BookMsg BookEvent

serverName :: ServerName State Msg
serverName = Local $ atom "handle_info_example"

-- Rather than using the default startLink, we use buildStartLink so we can provide
-- our own handleInfo function
startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args = Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: BookWatchingStartArgs -> Gen.Init State Msg
init args = do
  -- We can get hold of 'self', which is of type (Process Msg) as defined by serverName
  -- Anything sent to this will appear in our handleInfo (ooh typing)
  -- We can use this in callbacks to get messages back to us of the right type
  self <- Gen.self
  -- Speaking of, SimpleBus.subscribe is such a thing, given a bus name
  -- and an emitter, we'll be given messages of the right type
  _ <- Gen.lift $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self
  pure $ {}

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state = do
  case msg of
    -- And that means we can switch over the messages as they come in
    -- All the typing..
    BookMsg bookEvent -> Gen.lift $ handleBookEvent bookEvent state

-- And all we will do is log that we saw the events
-- as this demo ends here
handleBookEvent :: BookEvent -> State -> Effect (CastResult State)
handleBookEvent ev state = case ev of
  BookCreated isbn -> do
    _ <- logInfo "Book created" isbn
    pure $ CastNoReply state
  BookDeleted isbn -> do
    _ <- logInfo "Book deleted" isbn
    pure $ CastNoReply state
  BookUpdated isbn -> do
    _ <- logInfo "Book updated" isbn
    pure $ CastNoReply state

domain :: List Atom
domain = (atom "demo_ps") : (atom "handle_info_example") : nil

logInfo :: String -> Isbn -> Effect Unit
logInfo text isbn = Logger.info { domain, text, isbn, type: Logger.Trace } {}
