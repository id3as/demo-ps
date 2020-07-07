module BookLibrary where

import Prelude

import Books (Book,  Isbn,  BookEvent(..))
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Redis (ConnectionString, DbId, RedisConnection)
import Redis as Redis
import SimpleBus (Bus, bus, raise) as SimpleBus


-- Obviously this is optional, one could simply pass in "unit" if there were no args
type BookLibraryStartArgs = {
  connectionString :: ConnectionString
}

type State = {
  connection :: RedisConnection
}


bus :: SimpleBus.Bus String BookEvent
bus = SimpleBus.bus "book_library"

dbPrefix :: String
dbPrefix = "books:"

dbId :: Isbn -> DbId
dbId isbn = wrap $ dbPrefix <> unwrap isbn

-- We pass this into every interaction with Gen so we know what gen server we're talking about
-- The type of "State" is encoded into it so all callbacks are strongly typed around that
serverName :: ServerName State Unit
serverName = Local $ atom "book_library"

-- Note: It makes little sense *really* to hide a (largely) stateless connection
-- behind a gen-server, unless you *really* want a single R/W sync point
-- around a whole slew of objects, but this demonstrates that we at least have a gen server
-- And we can call into it and do things with its state (also available is cast, and pure versions of these calls
create :: Book -> Effect (Either String Book)
create book = 
  Gen.call serverName \state@{ connection } -> do
    existing <- Gen.lift $ Redis.get (dbId $ book.isbn) connection
    case existing of
         Nothing -> do
           Gen.lift $ Redis.put (dbId $ book.isbn) book connection
           Gen.lift $ SimpleBus.raise bus (BookCreated book.isbn)
           pure $ CallReply (Right book) state
         Just (gasp :: Book) -> 
           pure $ CallReply (Left "Book with this ISBN already exists") state


update :: Book -> Effect (Either String Book)
update book = 
  Gen.call serverName \state@{ connection } -> do
    Gen.lift $ Redis.put (dbId $ book.isbn) book connection
    Gen.lift $ SimpleBus.raise bus (BookUpdated book.isbn)
    pure $ CallReply (Right book) state

delete :: Isbn -> Effect Unit
delete isbn = 
  Gen.call serverName \state@{ connection } -> do
    Gen.lift $ Redis.delete (dbId isbn) connection
    Gen.lift $ SimpleBus.raise bus (BookDeleted isbn)
    pure $ CallReply unit state

findByIsbn :: Isbn -> Effect (Maybe Book)
findByIsbn isbn = 
  Gen.call serverName \state@{ connection } -> do
    result <- Gen.lift $ Redis.get (dbId isbn) connection
    pure $ CallReply result state

findAll :: Effect (List Book)
findAll = 
  Gen.call serverName \state@{ connection } -> do
    books <- Gen.lift $ Redis.findAll dbPrefix connection
    pure $ CallReply books state

-- Nothing special about this, just a function that returns a certain type
-- We can supply arbitrary arguments to this via the gensup
startLink :: BookLibraryStartArgs -> Effect StartLinkResult
startLink args =
   Gen.startLink serverName (init args)

-- And those arguments can then end up in here, which just needs to return an effect of our State type
init :: BookLibraryStartArgs -> Gen.Init State  Unit
init args = do
  connection <- Gen.lift $ Redis.open args.connectionString
  pure $ { connection }
