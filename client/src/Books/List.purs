module BookClient.Books.List where

import Prelude

import Affjax as AX
import Affjax.RequestBody (string) as AXRequest
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXResponse
import Affjax.StatusCode (StatusCode(..))
import BookClient.Navigation (GlobalMessage(..), Route(..))
import BookClient.Shared (StatusMessage(..), loadItem, loadList, onClick, renderMessage, validationFor, warningMessage)
import Control.Apply (lift2)
import Data.Array (elem, filter, foldl, head, (:))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, hush)
import Data.HTTP.Method (Method(..))
import Data.Int (round, toNumber) as Int
import Data.Map (Map) as Data
import Data.Map (fromFoldable, isEmpty, unions) as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Books (Book)
import Simple.JSON (class ReadForeign, readJSON, writeJSON)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as MouseEvent


data Query a

data Action = Load 
  | Tick
  | EditBook Event Book 
  | AddBook Event Book
  | DeleteBook Event Book

type Slot = H.Slot Query GlobalMessage

type Model = { message :: StatusMessage, books :: Array Book }

type BooksInput = Unit

component :: H.Component HH.HTML Query BooksInput GlobalMessage Aff
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  { handleAction = handleAction
  , initialize = Just Load
  }
}
  where

  initialState :: BooksInput -> Model
  initialState _ = { message: NoMessage, books: [] }

  render :: Model -> H.ComponentHTML Action () Aff
  render _ = HH.div_ []

  handleAction :: Action -> H.HalogenM Model Action () GlobalMessage Aff Unit
  handleAction action =  do
     case action of
          Load -> pure unit
          Tick -> pure unit 
          EditBook ev book  -> pure unit
          AddBook ev book -> pure unit
          DeleteBook ev book -> pure unit

