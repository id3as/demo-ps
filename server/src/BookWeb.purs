module BookWeb
  ( startLink
  , init
  , serverName
  , State
  )
  where

import Prelude

import BookLibrary as BookLibrary
import Books (Book, Isbn)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, readBody, setBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (RestResult, StaticAssetLocation(..), StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)
import Routes as Routes

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


jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))
