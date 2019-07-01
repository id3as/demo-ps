module BookLibrary where

import Prelude

import Books (Book)
import Data.Array (elem)
import Data.Either (Either(..), hush)
import Data.Filterable (partition)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (any, sequence)
import Effect (Effect)
import Erl.Data.List (List, filter, mapMaybe, nil, (:))
import Erl.Data.Tuple (tuple2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Redis (ConnectionString(..), RedisConnection)
import Redis (put, open, get, findAll) as Redis

-- Obviously this is optional, one could simply pass in "unit" if there were no args
type BookLibraryStartArgs = {
  connectionString :: ConnectionString
}

type State = {
  connection :: RedisConnection
}

-- We pass this into every interaction with Gen so we know what gen server we're talking about
-- The type of "State" is encoded into it so all callbacks are strongly typed around that
serverName :: ServerName State
serverName = ServerName "book_library"

-- Note: It makes little sense *really* to hide a (largely) stateless connection
-- behind a gen-server, unless you *really* want a single R/W sync point
-- around a whole slew of objects, but this demonstrates that we at least have a gen server
-- And we can call into it and do things with its state (also available is cast, and pure versions of these calls
create :: Book -> Effect (Either String Book)
create book = 
  Gen.doCall serverName \state@{ connection } -> do
    Redis.put (wrap book.isbn) book connection
    pure $ CallReply (Right book) state

findByIsbn :: String -> Effect (Maybe Book)
findByIsbn isbn = 
  Gen.doCall serverName \state@{ connection } -> do
    result <- Redis.get (wrap isbn) connection
    pure $ CallReply result state

findAll :: Effect (List Book)
findAll = 
  Gen.doCall serverName \state@{ connection } -> do
    books <- Redis.findAll connection
    pure $ CallReply books state

-- Nothing special about this, just a function that returns a certain type
-- We can supply arbitrary arguments to this via the gensup
startLink :: BookLibraryStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

-- And those arguments can then end up in here, which just needs to return an effect of our State type
init :: BookLibraryStartArgs -> Effect State
init args = do
  connection <- Redis.open args.connectionString
  pure $ { connection }
