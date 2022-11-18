module BookWeb
  ( startLink
  , init
  , serverName
  , State
  ) where

import Prelude

import BookLibrary as BookLibrary
import Books (Book, Isbn, BookEvent)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (notMoved)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody, streamBody, streamReply, StatusCode(..))
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Kernel.Inet (Port(..), ip4)
import Erl.Process (self, send)
import Erl.Process.Raw (getPid)
import Logger as Logger
import MonitorExample as MonitorExample
import OneForOneSup as OneForOneSup
import Partial.Unsafe (unsafePartial)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Monitor as Monitor
import Routes as Routes
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import SimpleBus as SimpleBus
import Stetson (AcceptHandlerResult, Authorized(..), CowboyHandler(..), LoopCallResult(..), RestResult, StaticAssetLocation(..), StetsonHandler, acceptFailure, acceptSuccess)
import Stetson as Stetson
import Stetson.Loop as Loop
import Stetson.Rest as Rest
import Stetson.Types (routeHandler)
import Stetson.WebSocket as WebSocket
import Unsafe.Coerce (unsafeCoerce)

newtype State = State {}

type BookWebStartArgs = { webPort :: Int }

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = Local $ atom "book_web"

-- Yes we're housing a cowboy server behind a gen server
-- this isn't necessary but it's somewhere to keep it
startLink :: BookWebStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

init :: BookWebStartArgs -> GenServer.InitFn Unit Unit Unit State
init args = do
  -- This is pretty self explanatory, Stetson.configure kicks off
  -- the process and then we can tweak that object by pipng it through
  void
    $ GenServer.liftEffect
    $ Stetson.startClear "http_listener"
    $ Stetson.configure
        { routes =
            Stetson.routes2
              -- These routes are defined in a typed object
              -- that dictate
              -- a) What paths to reach them on
              -- b) What arguments they expect (typed(!!))
              -- So the callbacks to these names are typed and can be referred to in shared/Routes.purs
              Routes.apiRoute
              { "Book": book
              , "Books": books
              , "EventsWs": eventsWs
              , "EventsFirehoseRest": eventsFirehoseRest
              , "DataStream": dataStream
              , "OneForOne": oneForOne
              , "Assets": PrivDir "demo_ps" "www/assets"
              , "Index": PrivFile "demo_ps" "www/index.html"
              , "Index2": (\(_ :: String) -> PrivFile "demo_ps" "www/index.html")
              }
        , bindPort = (Port args.webPort)
        , bindAddress = unsafePartial $ fromJust $ ip4 0 0 0 0
        }
  pure $ InitOk $ State {}

-- A plain ol' Handler that operates over a state of type 'List Book'
-- All the possible callbacks are defined as an example, generally these are not required
books :: StetsonHandler Unit (List Book)
books =
  -- This is effectively cowboy_rest
  routeHandler
    { init:
        \req -> do
          -- And our state can just be all the books in the library
          state <- BookLibrary.findAll
          void $ Logger.info { domain: (nil), text: "Current library", state: state, type: Logger.Trace } {}
          -- Return our unmodified req along with our state
          Rest.initResult req $ state
    , allowedMethods: \req state -> Rest.result (Stetson.POST : Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state
    -- This provides a list of Tuple2 ContentType Handler.  We only return json
    , contentTypesProvided: \req state -> Rest.result (jsonWriter : nil) req state
    -- And this is the equivalent list of handlers for PUSH/POST
    , contentTypesAccepted: \req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil) req state
    , allowMissingPost: \req state -> Rest.result false req state
    , forbidden: \req state -> Rest.result false req state
    , isAuthorized: \req state -> Rest.result Authorized req state
    , isConflict: \req state -> Rest.result false req state
    , malformedRequest: \req state -> Rest.result false req state
    , movedPermanently: \req state -> Rest.result notMoved req state
    , movedTemporarily: \req state -> Rest.result notMoved req state
    , previouslyExisted: \req state -> Rest.result false req state
    , resourceExists: \req state -> Rest.result true req state
    , serviceAvailable: \req state -> Rest.result true req state
    }
  where
  acceptJson :: forall state. Req -> state -> Effect (RestResult AcceptHandlerResult state)
  acceptJson req state = do
    -- Read the whole body, no buffering (how big can a book be??)
    body <- allBody req mempty
    -- read it as JSON, and chuck it into the create function of bookLibrary and obviously this is all
    -- either (Left/Right) all the way down
    result <- either (pure <<< Left <<< show) BookLibrary.create $ readJSON $ unsafeCoerce body
    case result of
      -- The point being that Left -> Failure -> False -> Err as the body
      Left err -> Rest.result acceptFailure (setBody err req) state
      -- And Right -> Success -> True and no body
      Right c -> Rest.result acceptSuccess req state

-- Another rest handler, this time of 'Maybe Book'
-- This is because we take in an Isbn (Look at that!! It just works thanks to the router)
-- And try and load the book which may or may not exist
book :: Isbn -> StetsonHandler Unit (Maybe Book)
book id =
  routeHandler
    { init:
        \req -> do
          --  Conveniently typed, and the Maybe just goes into state
          book' <- BookLibrary.findByIsbn id
          Rest.initResult req $ book'
    , allowedMethods: restHandler (Stetson.HEAD : Stetson.PUT : Stetson.DELETE : Stetson.GET : Stetson.OPTIONS : nil)
    -- If the book was loaded in init, state will be Just ...
    , resourceExists:
        \req state ->
          Rest.result (isJust state)
            (maybe (setBody "This book does not exist" req) (\_ -> req) state)
            state
    , deleteResource:
        \req state -> do
          void $ maybe (pure unit) (\book' -> BookLibrary.delete book'.isbn) state
          Rest.result true req state
    -- And the same trick as bove, sharing the jsonWriter for dumping the json out over the wire
    , contentTypesAccepted: (Rest.result (singleton $ tuple2 "application/json" acceptJson))
    , contentTypesProvided: (\req state -> Rest.result (jsonWriter : nil) req state)
    }
  where
  acceptJson req state = do
    body <- allBody req mempty
    result <- either (pure <<< Left <<< show) BookLibrary.update $ readJSON $ binaryToString body
    case result of
      Left err -> Rest.result acceptFailure (setBody err req) state
      Right c -> Rest.result acceptSuccess req state

-- We don't need this message type, but
-- We may as well define one as it means we can easily add more messages if we want to in the future
data EventsWsMsg = BookMsg BookEvent

-- This is a receiving  handler, which receives the message  typr defined above, and holds a state of 'Unit'
eventsWs :: StetsonHandler EventsWsMsg Unit
eventsWs =
  routeHandler
    { init
    , wsInit: wsInit
    , wsHandle: wsHandle
    , wsInfo: wsInfo
    }
  where
  -- init runs in a different process to the ws handler, so probably just run the default handler here
  init req = WebSocket.initResult req unit

  -- Subscribe to any events here, lift the messages into the right type if necessary
  -- emitter is of type (msg -> Effect Unit), anything passed into that will appear in .info
  wsInit s = do
    -- Get our pid
    self <- self
    -- Subscribe to the bus, and redirect the events into our emitter after wrapping them in our type
    void $ SimpleBus.subscribe BookLibrary.bus BookMsg
    pure $ Stetson.NoReply s

  -- Receives 'Frame' sent from client (text,ping,binary,etc)
  wsHandle frame state = pure $ Stetson.NoReply state

  -- Receives messages that were sent into 'emitter', typically so they can then be 'Replied' into the websocket
  wsInfo (BookMsg msg) state = pure $ Stetson.Reply ((TextFrame $ writeJSON msg) : nil) state

-- This is a handler that starts off being a RestHandler
-- but then switches into a LoopHandler for streaming the data once Conneg has taken place
eventsFirehoseRest :: StetsonHandler EventsWsMsg Unit
eventsFirehoseRest =
  routeHandler
    { init: \req -> Rest.initResult req unit
    , allowedMethods: allowedMethods
    , contentTypesProvided: contentTypesProvided
    , loopInit
    , loopInfo
    }
  where
  allowedMethods req state = Rest.result (Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state

  -- And we'll say "hey, we provide this type of data" (which is application/json)
  -- The next thing to read is the streamEvents function below if we're being chronological
  contentTypesProvided req state = Rest.result (streamEvents : nil) req state

  -- Once we've switched to the Loop handler, we'll be given a chance to register
  -- any callbacks with the emitter for this handler
  loopInit req state = do
    -- Get 'self' out of the state monad!!!!!!!!
    self <- self
    -- In this case, we'll subscribe to the bus and throw any messages it sends us
    -- into Book Msg
    -- We do need to lift the effect into our StateT tho
    void $ SimpleBus.subscribe BookLibrary.bus BookMsg
    pure state

  -- And then those messages will appear here so we can
  loopInfo (BookMsg msg) req state = do
    -- Stream them to the client as we get them
    void $ liftEffect $ streamBody (stringToIOData $ writeJSON msg) req
    -- And keep on loopin'
    pure $ LoopOk req state

  -- So we provide application/json, and if they choose to take that then
  -- we'll call streamReply on Cowboy to let it know that's what we're doing
  streamEvents =
    tuple2 "application/json"
      ( \req state -> do
          req2 <- streamReply (StatusCode 200) Map.empty req
          -- And then we'll switch to the LoopHandler (head back up to Loop.init)
          Rest.switchHandler LoopHandler req2 state
      )

-- And see here how we needed some  more messages
data DataStreamMessage
  = Data Binary
  | DataSourceDied
  | DataSourceAlreadyDown

-- This is a handler analogous to cowboy_loop
dataStream :: StetsonHandler DataStreamMessage Unit
dataStream =
  routeHandler
    { init
    , loopInit: loopInit
    , loopInfo: loopInfo
    }
  where
  init req = do
    -- Start off by initting the streamed  response, no headers, lazy
    req2 <- streamReply (StatusCode 200) Map.empty req
    -- And return the Loop handler
    Loop.initResult req2 unit

  -- Now we've decided to be a loop handler, we'll be given the emitter
  --into which we can pass messages that'll appear in 'info'
  loopInit req state = do
    -- Get our typed pid
    self <- self
    -- We'll register our emitter with the gen server
    -- It can then send us messages
    void $ liftEffect $ MonitorExample.registerClient $ send self <<< Data

    maybePid <- liftEffect $ GenServer.whereIs (MonitorExample.serverName)

    case maybePid of
      Just pid -> do
        -- But we'll also add a monitor to that gen server so we know if it dies
        -- There are two messages here, we could just use the same one but I want the example to be clear
        void $ liftEffect $ Monitor.monitorTo pid self (const DataSourceDied)
        pure unit
      _ -> do
        liftEffect $ send self DataSourceAlreadyDown
        pure unit

  -- If we receive a message from the gen server
  loopInfo msg req state = do
    case msg of
      Data binary -> do
        -- Then stream that down to the client
        void $ liftEffect $ streamBody (IOData.fromBinary binary) req
        -- And LoopOk cos we'll wait for the next message
        pure $ LoopOk req state
      DataSourceDied -> do
        -- then terminate the stream
        pure $ LoopStop req state
      DataSourceAlreadyDown -> do
        -- and here too
        pure $ LoopStop req state

data OneForOneMessage = OfOData Binary

oneForOne :: StetsonHandler OneForOneMessage Unit
oneForOne =
  routeHandler
    { init:
        \req -> do
          -- Start off by initting the streamed response, no headers, lazy
          req2 <- streamReply (StatusCode 200) Map.empty req
          -- And return the Loop handler
          Loop.initResult req2 unit
    , loopInit: loopInit
    , loopInfo: loopInfo
    }
  where
  loopInit req state = do
    -- Get our typed pid
    self <- self
    -- We'll register our emitter with the gen server
    -- It can then send us messages
    void $ liftEffect $ OneForOneSup.startClient { handler: send self <<< OfOData, clientPid: getPid self }
    -- And carry on
    pure state

  -- If we receive a message from the gen server
  loopInfo msg req state = do
    case msg of
      OfOData binary -> do
        -- Then stream that down to the client
        void $ liftEffect $ streamBody (IOData.fromBinary binary) req
        -- And LoopOk cos we'll wait for the next message
        pure $ LoopOk req state

jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult IOData a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (stringToIOData $ writeJSON state) req state)

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
    (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
    (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

-- Simple rest response
restHandler :: forall responseType state. responseType -> Req -> state -> Effect (RestResult responseType state)
restHandler val req state = Rest.result val req state

-- This is just one of those things that hasn't become painful enough to resolve yet
-- All strings are binaries
-- but not all binaries are strings
-- in theory this unsafeCoerce is therefore a bad thing to be doing
-- but in Erlang world  we'd have just assumed  it was a binary string anyway
-- there is no great story around this yet
stringToIOData :: String -> IOData
stringToIOData = IOData.fromBinary <<< unsafeCoerce

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
