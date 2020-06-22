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

newtype State = State {}

type BookWebStartArgs = { webPort :: Int }

serverName :: ServerName State Unit
serverName = Local $ atom "book_web"

startLink :: BookWebStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: BookWebStartArgs -> Effect State
init args = do
  _ <- Stetson.configure
    # Stetson.routes 
      Routes.apiRoute {
          "Book": book
        , "Books": books
        , "EventsWs": eventsWs
        , "EventsFirehoseRest": eventsFirehoseRest
        , "EventsFirehoseStream": eventsFirehoseStream
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

eventsFirehoseRest :: ReceivingStetsonHandler EventsWsMsg Unit
eventsFirehoseRest =
  emptyHandler (\req -> Rest.initResult req unit)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.POST :  Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (streamEvents : nil) req state)
    # Loop.init (\emitter req state -> do
                              _ <- SimpleBus.subscribe BookLibrary.bus (\ev -> emitter $ BookMsg ev)
                              pure state)
    # Loop.info (\(BookMsg msg) req state ->  do
          _ <- Logger.info1 "Sending ~p" msg
          _ <- streamBody (stringToBinary $ writeJSON msg) req
          pure $ LoopOk req state)
    # Rest.yeeha
    where 
          streamEvents = tuple2 "application/json" (\req state -> do
                         req2 <- streamReply (StatusCode 200) Map.empty req
                         Rest.switchHandler LoopHandler req2 state)
                                           
eventsFirehoseStream :: ReceivingStetsonHandler EventsWsMsg Unit
eventsFirehoseStream =
  Loop.handler (\req -> do
               req2 <- streamReply (StatusCode 200) Map.empty req
               Loop.initResult req2 unit)
    # Loop.init (\emitter req state -> do
                              _ <- SimpleBus.subscribe BookLibrary.bus (\ev -> emitter $ BookMsg ev)
                              pure state)
    # Loop.info (\(BookMsg msg) req state ->  do
          _ <- Logger.info1 "Sending ~p" msg
          _ <- streamBody (stringToBinary $ writeJSON msg) req
          pure $ LoopOk req state)
    # Rest.yeeha


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
