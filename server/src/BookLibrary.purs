module BookLibrary where

import Prelude

import Books (Book, Isbn, BookEvent(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List)
import Pinto (StartLinkResult, RegistryName(..), RegistryReference(..))
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Redis (ConnectionString, DbId, RedisConnection)
import Redis as Redis
import SimpleBus (Bus, bus, raise) as SimpleBus

-- Obviously this is optional, one could simply pass in "unit" if there were no args
type BookLibraryStartArgs
  = { connectionString :: ConnectionString
    }

type State
  = { connection :: RedisConnection
    }

bus :: SimpleBus.Bus String BookEvent
bus = SimpleBus.bus "book_library"

dbPrefix :: String
dbPrefix = "books:"

dbId :: Isbn -> DbId
dbId isbn = wrap $ dbPrefix <> unwrap isbn

-- We pass this into every interaction with GenServer so we know what gen server we're talking about
-- The type of "State" is encoded into it so all callbacks are strongly typed around that
serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = Local $ atom "book_library"

-- Note: It makes little sense *really* to hide a (largely) stateless connection
-- behind a gen-server, unless you *really* want a single R/W sync point
-- around a whole slew of objects, but this demonstrates that we at least have a gen server
-- And we can call into it and do things with its state (also available is cast, and pure versions of these calls
create :: Book -> Effect (Either String Book)
create book =
  GenServer.call (ByName serverName) \_from state@{ connection } -> do
    existing <- GenServer.liftEffect $ Redis.get (dbId $ book.isbn) connection
    case existing of
      Nothing -> do
        GenServer.liftEffect $ Redis.put (dbId $ book.isbn) book connection
        GenServer.liftEffect $ SimpleBus.raise bus (BookCreated book.isbn)
        pure $ GenServer.reply (Right book) state
      Just (_gasp :: Book) -> pure $ GenServer.reply (Left "Book with this ISBN already exists") state

update :: Book -> Effect (Either String Book)
update book =
  GenServer.call (ByName serverName) \_from state@{ connection } -> do
    GenServer.liftEffect $ Redis.put (dbId $ book.isbn) book connection
    GenServer.liftEffect $ SimpleBus.raise bus (BookUpdated book.isbn)
    pure $ GenServer.reply (Right book) state

delete :: Isbn -> Effect Unit
delete isbn =
  GenServer.call (ByName serverName) \_from state@{ connection } -> do
    GenServer.liftEffect $ Redis.delete (dbId isbn) connection
    GenServer.liftEffect $ SimpleBus.raise bus (BookDeleted isbn)
    pure $ GenServer.reply unit state

findByIsbn :: Isbn -> Effect (Maybe Book)
findByIsbn isbn =
  GenServer.call (ByName serverName) \_from state@{ connection } -> do
    result <- GenServer.liftEffect $ Redis.get (dbId isbn) connection
    pure $ GenServer.reply result state

findAll :: Effect (List Book)
findAll =
  GenServer.call (ByName serverName) \_from state@{ connection } -> do
    books <- GenServer.liftEffect $ Redis.findAll dbPrefix connection
    pure $ GenServer.reply books state

-- Nothing special about this, just a function that returns a certain type
-- We can supply arbitrary arguments to this via the gensup
startLink :: BookLibraryStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

init :: BookLibraryStartArgs -> GenServer.InitFn Unit Unit Unit State
init args = do
  connection <- GenServer.liftEffect $ Redis.open args.connectionString
  pure $ InitOk { connection }
