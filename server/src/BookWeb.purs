module BookWeb
  ( startLink
  , init
  , serverName
  , State
  )
  where

import Prelude

import BookLibrary as BookLibrary
import Books (Book, Isbn, BookEvent(..))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Map as Map
import Erl.Process (send)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, readBody, setBody, streamReply, streamBody, StatusCode(..) )
import Erl.Data.Binary (Binary)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (RestResult, StaticAssetLocation(..), SimpleStetsonHandler, StetsonHandler,  emptyHandler, CowboyHandler(..), LoopCallResult(..))
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Loop as Loop
import Stetson.WebSocket as WebSocket
import Unsafe.Coerce (unsafeCoerce)
import Routes as Routes
import SimpleBus as SimpleBus
import Logger as Logger
import MonitorExample as MonitorExample

newtype State = State {}

type BookWebStartArgs = { webPort :: Int }

serverName :: ServerName State Unit
serverName = Local $ atom "book_web"

-- Yes we're housing a cowboy server behind a gen server
-- this isn't necessary but it's somewhere to keep it
startLink :: BookWebStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) 

init :: BookWebStartArgs -> Gen.Init State Unit
init args = do
  -- This is pretty self explanatory, Stetson.configure kicks off 
  -- the process and then we can tweak that object by pipng it through
  _ <- Gen.lift $ 
    Stetson.configure
    # Stetson.routes 
      -- These routes are defined in a typed object
      -- that dictate 
      -- a) What paths to reach them on
      -- b) What arguments they expect (typed(!!))
      -- So the callbacks to these names are typed and can be referred to in shared/Routes.purs
      Routes.apiRoute {
          "Book": book
        , "Books": books
        , "EventsWs": eventsWs
        , "EventsFirehoseRest": eventsFirehoseRest
        , "DataStream": dataStream
        , "Assets": PrivDir "demo_ps" "www/assets"
        , "Index": PrivFile "demo_ps" "www/index.html"
        , "Index2": (\(_ :: String)  -> PrivFile "demo_ps" "www/index.html")
      }
    # Stetson.port args.webPort
    # Stetson.bindTo 0 0 0 0
    # Stetson.startClear "http_listener"
  pure $ State {}

-- A plain ol' Handler that operates over a state of type 'List Book'
books :: SimpleStetsonHandler (List Book)
books =
  -- We kick it off from the Rest namespace, so this is effectively cowboy_rest
  Rest.handler (\req -> do

                        -- And  our state can just be all the books in the library
                        state <- BookLibrary.findAll

                        -- Return our unmodified req along with our state
                        Rest.initResult req state)

    -- Standard read/write methods
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.POST :  Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)

    -- We provide/accept json, both of those are just callbacks
    -- You can see how we'd use composition to just have a standard mechanism for returning json from an 'object'
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil)
                                req state)
    where 
          acceptJson req state = do
             
            -- Read the whole body, no buffering (how big can a book be??)
            body <- allBody req mempty

            -- read it as JSON, and chuck it into the create function of bookLibrary and obviously this is all
            -- either (Left/Right) all the way down
            result <- either (pure <<< Left <<< show) BookLibrary.create $ readJSON $ unsafeCoerce body
            case result of
                 -- The point being that Left -> Failure -> False -> Err as the body
                 Left err -> Rest.result false (setBody err req) state

                 -- And Right -> Success -> True and no body
                 Right c -> Rest.result true req state
                                           
-- Another rest handler, this time of 'Maybe Book'
-- This is because we take in an Isbn (Look at that!! It just works thanks to the router)
-- And try and load the book which may or may not exist
book :: Isbn -> SimpleStetsonHandler (Maybe Book)
book id = 
  Rest.handler (\req -> do
                          --  Conveniently typed, and the Maybe  just goes  into state
                          book <- BookLibrary.findByIsbn id
                          Rest.initResult req book)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.PUT : Stetson.DELETE : Stetson.GET : Stetson.OPTIONS : nil) req state)

    -- the resourceExists if we have a Just...
    # Rest.resourceExists (\req state -> 
                             Rest.result (isJust state) 
                             (maybe (setBody "This book does not exist" req) (\_ -> req) state)
                             state)

    -- Deletion always succeeds
    # Rest.deleteResource (\req state -> do
                              _ <- maybe (pure unit) (\book -> BookLibrary.delete book.isbn) state
                              Rest.result true req state)

    -- And the same trick as bove, sharing the jsonWriter for dumping the json out over the wire
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil) req state)
    where
          acceptJson req state = do
             body <- allBody req mempty
             result <- either (pure <<< Left <<< show) BookLibrary.update $ readJSON $ binaryToString body
             case result of
                  Left err -> Rest.result false (setBody err req) state
                  Right c -> Rest.result true req state


-- We don't need this message type, but 
-- We may as well define one as it means we can easily add more messages if we want to in the future
data EventsWsMsg = BookMsg BookEvent

-- This is a receiving  handler, which receives the message  typr defined above, and holds a state of 'Unit'
eventsWs :: StetsonHandler EventsWsMsg Unit
eventsWs =

  -- Pull your bindings out of req here and create an initial state
  WebSocket.handler (\req -> WebSocket.initResult req unit)

  -- Subscribe to any events here, lift the messages into the right type if necessary
  -- emitter is of type (msg -> Effect Unit), anything passed into that will appear in .info
  # WebSocket.init (\s ->  do
                              -- Get our pid
                              self <- WebSocket.self

                               -- Subscribe to the bus, and redirect the events into our emitter after wrapping them in our type 
                              _ <- WebSocket.lift $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self

                              pure $ Stetson.NoReply s
                             )

  -- Receives 'Frame' sent from client (text,ping,binary,etc)
  # WebSocket.handle (\frame state -> pure $ Stetson.NoReply state)

  -- Receives messages that were sent into 'emitter', typically so they can then be 'Replied' into the websocket
  # WebSocket.info (\(BookMsg msg) state -> pure $ Stetson.Reply ((TextFrame $ writeJSON msg) : nil) state)

-- This is a handler that starts off being a RestHandler
-- but then switches into a LoopHandler for streaming the data once Conneg has taken place
eventsFirehoseRest :: StetsonHandler EventsWsMsg Unit
eventsFirehoseRest =
  -- So empty handler that returns a Rest.initResult
  emptyHandler (\req -> Rest.initResult req unit)

    -- Standard read-only headers
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)

    -- And we'll say "hey, we provide this type of data" (which is application/json)
    -- The next thing to read is the streamEvents function below if we're being chronological
    # Rest.contentTypesProvided (\req state -> Rest.result (streamEvents : nil) req state)

    -- Once we've switched to the Loop handler, we'll be given a chance to register
    -- any callbacks with the emitter for this handler
    # Loop.init (\req state -> do

                              -- Get 'self' out of the state monad!!!!!!!!
                              self <- Loop.self

                              -- In this case, we'll subscribe to the bus and throw any messages it sends us 
                              -- into Book Msg
                              -- We do need to lift the effect into our StateT tho
                              _ <- Loop.lift $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self

                              pure state)

    -- And then those messages will appear here so we can
    # Loop.info (\(BookMsg msg) req state ->  do

          -- Stream them to the client as we get them
          _ <- Loop.lift $ streamBody (stringToBinary $ writeJSON msg) req
          
          -- And keep on loopin'
          pure $ LoopOk req state)

    where 
          -- So we provide application/json, and if they choose to take that then
          -- we'll call streamReply on Cowboy to let it know that's what we're doing
          streamEvents = tuple2 "application/json" (\req state -> do
                         req2 <- streamReply (StatusCode 200) Map.empty req

                         -- And then we'll switch to the LoopHandler (head back up to Loop.init)
                         Rest.switchHandler LoopHandler req2 state)


-- And see here how we needed some  more messages
data DataStreamMessage = Data Binary
                       | DataSourceDied
                       | DataSourceAlreadyDown
                        
                                           
-- This is a handler analogous to cowboy_loop
dataStream :: StetsonHandler DataStreamMessage Unit
dataStream =
  Loop.handler (\req -> do

               -- Start off by initting the streamed  response, no headers, lazy
               req2 <- streamReply (StatusCode 200) Map.empty req

               -- And return the Loop handler
               Loop.initResult req2 unit)

    -- Now we've decided to be a loop handler, we'll be given the emitter
    -- into which we can pass messages that'll appear in 'info'
    # Loop.init (\req state -> do 
 
                      -- Get our typed pid
                      self <- Loop.self

                      -- We'll register our emitter with the gen server
                      -- It can then send us messages
                      void $ Loop.lift $  MonitorExample.registerClient $ send self <<< Data

                      -- But we'll also add a monitor to that gen server so we know if it dies
                      -- There are two messages here, we could just use the same one but I want the example to be clear
                      void $ Loop.lift $ Gen.monitor MonitorExample.serverName (\_ -> send self DataSourceDied) (send self DataSourceAlreadyDown)

                      -- And carry on
                      pure unit)

    -- If we receive a message from the gen server
    # Loop.info (\msg req state ->  do
                case msg of
                     Data binary -> do

                        -- Then stream that down to the client
                        _ <- Loop.lift $ streamBody binary req

                        -- And LoopOk cos we'll wait for the next message
                        pure $ LoopOk req state

                     DataSourceDied ->  do

                       -- then terminate the stream
                       pure $ LoopStop req state

                     DataSourceAlreadyDown ->  do

                       -- and here too
                       pure $ LoopStop req state
                 )



jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))


-- This is just one of those things that hasn't become painful enough to resolve yet
-- All strings are binaries
-- but not all binaries are strings
-- in theory this unsafeCoerce is therefore a bad thing to be doing
-- but in Erlang world  we'd have just assumed  it was a binary string anyway
-- there is no great story around this yet
stringToBinary :: String -> Binary
stringToBinary = unsafeCoerce

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
