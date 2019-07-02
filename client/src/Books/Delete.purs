module BookClient.Books.Delete where

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXResponse
import Affjax.StatusCode (StatusCode(..))
import BookClient.Books.Shared (BookInput)
import BookClient.Navigation (GlobalMessage(..), Route(..))
import BookClient.Shared (StatusMessage(..), loadItem, renderMessage, warningMessage)
import Books (Book)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (toEvent) as MouseEvent

data Query a

data Action = Load
  | DeleteBook Event 
  | BackToListView Event

type Slot = H.Slot Query GlobalMessage

type Model = { message :: StatusMessage, posting :: Boolean, book :: Book }

type ActionHandler = H.HalogenM Model Action () GlobalMessage Aff Unit
type RenderHandler = H.ComponentHTML Action () Aff

component :: H.Component HH.HTML Query BookInput GlobalMessage Aff
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

  initialState :: BookInput -> Model
  initialState isbn = { message: NoMessage, posting: false, book: { isbn: unwrap isbn, title: "", author: "" } }

  render :: Model -> RenderHandler
  render { message, book, posting } = 
    HH.div [] [ renderMessage message 
              , HH.p [] [ HH.text "Are you sure you want to delete this really important book?" ]
              , HH.div [] 
                [ HH.div [ HP.class_ B.formGroup ] 
                    [ HH.label [ HP.for "isbn" ] [ HH.text "Isbn" ]
                    , HH.input [ HP.class_ B.formControl, HP.id_ "isbn", HP.value $ book.isbn, HP.readOnly true ] 
                    ]
                , HH.div [ HP.class_ B.formGroup ] 
                    [ HH.label [ HP.for "title" ] [ HH.text "Title" ]
                    , HH.input [ HP.class_ B.formControl, HP.id_ "title", HP.value $ book.title, HP.readOnly true ] 
                    ]
                , HH.div [ HP.class_ B.formGroup ] 
                    [ HH.label [ HP.for "author" ] [ HH.text "Author" ]
                    , HH.input [ HP.class_ B.formControl, HP.id_ "author", HP.value $ book.author, HP.readOnly true ] 
                    ]
                , if not posting then HH.div [] 
                    [ HH.button [ HP.classes [ B.btn, B.btnPrimary ], HE.onClick (\e -> Just $ DeleteBook (MouseEvent.toEvent e)) ] [ HH.text "Delete" ]
                    , HH.button [ HP.classes [ B.btn, B.btnSecondary ], HE.onClick (\e -> Just $ BackToListView (MouseEvent.toEvent e)) ] [ HH.text "Cancel" ]
                    ]
                  else HH.span [] []
                ]
              ]

  handleAction :: Action -> ActionHandler
  handleAction action =  do
     case action of
          Load -> loadBook 
          DeleteBook ev -> do
            H.liftEffect $ preventDefault ev 
            deleteBook
          BackToListView ev -> do
            H.liftEffect $ preventDefault ev 
            H.raise $ NavigateToRoute BooksIndex


loadBook :: ActionHandler
loadBook = do
  state@{ book: { isbn } } <- H.get
  maybeBook <- H.liftAff $ loadItem $ "/api/books/" <> isbn
  case maybeBook of
       Left _ -> do
         H.raise $ NavigateToRoute BooksIndex
       Right book -> do
         H.put $ state { book = book }
deleteBook :: ActionHandler
deleteBook = do
  state@{ book } <- H.get
  _ <- H.put state { posting = true }
  response <- H.liftAff $ AX.request $ (AX.defaultRequest 
               { url = "/api/books/" <>  book.isbn
               , method = Left DELETE
               , headers = [ ContentType $ MediaType "application/json" ]
               , responseFormat = AXResponse.string  
               })
  case response.status of
    StatusCode 204 -> do
     H.raise $ NavigateToRoute BooksIndex
    _ ->  
      H.put $ state { posting = false, message = warningMessage $ either (\_ -> "Unknown Error") identity response.body  }
