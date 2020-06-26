module HandleInfoExample where

import Prelude

import Books (Book,  Isbn, BookEvent(..))
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
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

type BookWatchingStartArgs = {}
type State = {}

data Msg = BookMsg BookEvent 

serverName :: ServerName State Msg
serverName = Local $ atom "handle_info_example"

-- Rather than using the default startLink, we use buildStartLink so we can provide
-- our own handleInfo function
startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: BookWatchingStartArgs -> Effect State
init args = do
  -- We can get hold of 'self', which is of type (Process Msg) as defined by serverName
  -- Anything sent to this will appear in our handleInfo (ooh typing)
  -- We can use this in callbacks to get messages back to us of the right type
  self <- Gen.self serverName 

  -- Speaking of, SimpleBus.subscribe is such a thing, given a bus name
  -- and an emitter, we'll be given messages of the right type 
  _ <- SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self

  pure $ {}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = do
  case msg of
    -- And that means we can switch over the messages as they come in
    -- All the typing..
    BookMsg bookEvent -> handleBookEvent bookEvent state

-- And all we will do is log that we saw the events 
-- as this demo ends here
handleBookEvent :: BookEvent -> State -> Effect (CastResult State)
handleBookEvent ev state =
  case ev of
    BookCreated isbn -> do
      _ <- Logger.info1 "Book created ~p" isbn
      pure $ CastNoReply state
    BookDeleted isbn -> do
      _ <- Logger.info1 "Book deleted ~p" isbn
      pure $ CastNoReply state
    BookUpdated isbn -> do
      _ <- Logger.info1 "Book updated ~p" isbn
      pure $ CastNoReply state


