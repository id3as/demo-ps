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
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, readBody, setBody, streamReply, streamBody, StatusCode(..) )
import Erl.Data.Binary (Binary)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (RestResult, StaticAssetLocation(..), StetsonHandler, ReceivingStetsonHandler, emptyHandler, CowboyHandler(..), LoopCallResult(..))
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

startLink :: BookWebStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) 

init :: BookWebStartArgs -> Effect State
init args = do
  _ <- Stetson.configure
    # Stetson.routes 
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

books :: StetsonHandler (List Book)
books =
  Rest.handler (\req -> do
                        state <- BookLibrary.findAll
                        Rest.initResult req state)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.POST :  Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil)
                                req state)
    # Rest.yeeha
    where 
          acceptJson req state = do
            body <- allBody req mempty
            result <- either (pure <<< Left <<< show) BookLibrary.create $ readJSON $ unsafeCoerce body
            case result of
                 Left err -> Rest.result false (setBody err req) state
                 Right c -> Rest.result true req state
                                           
book :: Isbn -> StetsonHandler (Maybe Book)
book id = 
  Rest.handler (\req -> do
                          book <- BookLibrary.findByIsbn id
                          Rest.initResult req book)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.PUT : Stetson.DELETE : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.resourceExists (\req state -> 
                             Rest.result (isJust state) 
                             (maybe (setBody "This book does not exist" req) (\_ -> req) state)
                             state)
    # Rest.deleteResource (\req state -> do
                              _ <- maybe (pure unit) (\book -> BookLibrary.delete book.isbn) state
                              Rest.result true req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil) req state)
    # Rest.yeeha
    where
          acceptJson req state = do
             body <- allBody req mempty
             result <- either (pure <<< Left <<< show) BookLibrary.update $ readJSON $ unsafeCoerce body
             case result of
                  Left err -> Rest.result false (setBody err req) state
                  Right c -> Rest.result true req state


data EventsWsMsg = BookMsg BookEvent

eventsWs :: ReceivingStetsonHandler EventsWsMsg Unit
eventsWs =

  -- Pull your bindings out of req here and create an initial state
  WebSocket.handler (\req -> WebSocket.initResult req unit)

  -- Subscribe to any events here, lift the messages into the right type if necessary
  -- emitter is of type (msg -> Effect Unit), anything passed into that will appear in .info
  # WebSocket.init (\emitter s ->  do
                              _ <- SimpleBus.subscribe BookLibrary.bus (\ev -> emitter $ BookMsg ev)
                              pure $ Stetson.NoReply s
                             )

  -- Receives 'Frame' sent from client (text,ping,binary,etc)
  # WebSocket.handle (\frame state -> pure $ Stetson.NoReply state)

  -- Receives messages that were sent into 'emitter', typically so they can then be 'Replied' into the websocket
  # WebSocket.info (\(BookMsg msg) state -> pure $ Stetson.Reply ((TextFrame $ writeJSON msg) : nil) state)

  -- Yeeaha
  # WebSocket.yeeha

-- This is a handler that starts off being a RestHandler
-- but then switches into a LoopHandler for streaming the data once Conneg has taken place
eventsFirehoseRest :: ReceivingStetsonHandler EventsWsMsg Unit
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
    # Loop.init (\emitter req state -> do
                                
                              -- In this case, we'll subscribe to the bus and throw any messages it sends us 
                              -- into BookMsg
                              _ <- SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> emitter
                              pure state)

    -- And then those messages will appear here so we can
    # Loop.info (\(BookMsg msg) req state ->  do

          -- Stream them to the client as we get them
          _ <- streamBody (stringToBinary $ writeJSON msg) req
          
          -- And keep on loopin'
          pure $ LoopOk req state)

    # Loop.yeeha
    where 
          -- So we provide application/json, and if they choose to take that then
          -- we'll call streamReply on Cowboy to let it know that's what we're doing
          streamEvents = tuple2 "application/json" (\req state -> do
                         req2 <- streamReply (StatusCode 200) Map.empty req

                         -- And then we'll switch to the LoopHandler (head back up to Loop.init)
                         Rest.switchHandler LoopHandler req2 state)


data DataStreamMessage = Data Binary
                       | DataSourceDied
                       | DataSourceAlreadyDown
                        
                                           
-- This is a handler analogous to cowboy_loop
dataStream :: ReceivingStetsonHandler DataStreamMessage Unit
dataStream =
  Loop.handler (\req -> do

               -- Start off by initting the streamed  response, no headers, lazy
               req2 <- streamReply (StatusCode 200) Map.empty req

               -- And return the Loop handler
               Loop.initResult req2 unit)

    -- Now we've decided to be a loop handler, we'll be given the emitter
    -- into which we can pass messages that'll appear in 'info'
    # Loop.init (\emitter req state -> do 
                                        
                                        -- We'll register our emitter with the gen server
                                        -- It can then send us messages
                                        void $ MonitorExample.registerClient $ emitter <<< Data

                                        -- But we'll also add a monitor to that gen server so we know if it dies
                                        -- There are two messages here, we could just use the same one but I want the example to be clear
                                        void $ Gen.monitor MonitorExample.serverName (\_ -> emitter DataSourceDied) (emitter DataSourceAlreadyDown)

                                        -- And carry on
                                        pure unit)

    -- If we receive a message from the gen server
    # Loop.info (\msg req state ->  do
                case msg of
                     Data binary -> do

                        -- Then stream that down to the client
                        _ <- streamBody binary req

                        -- And LoopOk cos we'll wait for the next message
                        pure $ LoopOk req state

                     DataSourceDied ->  do

                       -- then terminate the stream
                       pure $ LoopStop req state

                     DataSourceAlreadyDown ->  do

                       -- and here too
                       pure $ LoopStop req state
                 )

    # Loop.yeeha


jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))



stringToBinary :: String -> Binary
stringToBinary = unsafeCoerce
