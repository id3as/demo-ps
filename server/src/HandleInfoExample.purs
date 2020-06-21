module HandleInfoExample where

import Prelude

import Books (Book,  Isbn)
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
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

data Msg = BookMsg BookLibrary.BookEvent 

serverName :: ServerName State Msg
serverName = Local $ atom "handle_info_example"

startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

init :: BookWatchingStartArgs -> Effect State
init args = do
  _ <- Logger.info1 "Handle info example  started ~p" ""
  SimpleBus.genSubscribe serverName BookLibrary.bus BookMsg
  pure $ {}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = do
  case msg of
    BookMsg bookEvent -> handleBookEvent bookEvent state

handleBookEvent :: BookLibrary.BookEvent -> State -> Effect (CastResult State)
handleBookEvent ev state =
  case ev of
    BookLibrary.BookCreated isbn -> do
      _ <- Logger.info1 "Book created ~p" isbn
      pure $ CastNoReply state
    BookLibrary.BookDeleted isbn -> do
      _ <- Logger.info1 "Book deleted ~p" isbn
      pure $ CastNoReply state
    BookLibrary.BookUpdated isbn -> do
      _ <- Logger.info1 "Book updated ~p" isbn
      pure $ CastNoReply state


