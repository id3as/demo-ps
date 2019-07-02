module BookClient.Books.Create where

import Prelude

import Affjax as AX
import Affjax.RequestBody (string) as AXRequest
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXResponse
import Affjax.StatusCode (StatusCode(..))
import BookClient.Books.Shared (validateBook)
import BookClient.Navigation (GlobalMessage(..), Route(..))
import BookClient.Shared (StatusMessage(..), ValidationMap, renderMessage, validationFor, warningMessage)
import Books (Book)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Map (isEmpty) as Map
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Simple.JSON (writeJSON)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (toEvent) as MouseEvent

data Query a

data Action = IsbnChanged String
  | TitleChanged String
  | AuthorChanged String
  | SaveNewBook Event 
  | BackToListView Event

type Slot = H.Slot Query GlobalMessage

type Model = { message :: StatusMessage, posting :: Boolean, book :: Book, validation :: ValidationMap }

type BooksInput = Unit

type ActionHandler = H.HalogenM Model Action () GlobalMessage Aff Unit
type RenderHandler = H.ComponentHTML Action () Aff

component :: H.Component HH.HTML Query BooksInput GlobalMessage Aff
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  { handleAction = handleAction
  }
}
  where

  initialState :: BooksInput -> Model
  initialState _ = { message: NoMessage, posting: false, book: { isbn: "", title: "", author: "" }, validation: mempty }

  render :: Model -> RenderHandler
  render { message, book, validation, posting } = 
    HH.div [] [ renderMessage message 
              , HH.div [] 
                [ HH.div [ HP.class_ B.formGroup ] 
                    [ HH.label [ HP.for "isbn" ] [ HH.text "Isbn" ]
                    , HH.input [ HP.class_ B.formControl, HP.id_ "isbn", HP.value $ book.isbn, HE.onValueInput (Just <<< IsbnChanged) ] 
                    , validationFor validation "isbn" "This is the registered ISBN of the book"
                    ]
                , HH.div [ HP.class_ B.formGroup ] 
                    [ HH.label [ HP.for "title" ] [ HH.text "Title" ]
                    , HH.input [ HP.class_ B.formControl, HP.id_ "title", HP.value $ book.title, HE.onValueInput (Just <<< TitleChanged) ] 
                    , validationFor validation "title" "I think this is self-explanatory no?"
                    ]
                , HH.div [ HP.class_ B.formGroup ] 
                    [ HH.label [ HP.for "author" ] [ HH.text "Author" ]
                    , HH.input [ HP.class_ B.formControl, HP.id_ "author", HP.value $ book.author, HE.onValueInput (Just <<< AuthorChanged) ] 
                    , validationFor validation "author" "As is this"
                    ]
                , if not posting then HH.div [] 
                    [ HH.button [ HP.classes [ B.btn, B.btnPrimary ], HE.onClick (\e -> Just $ SaveNewBook (MouseEvent.toEvent e)) ] [ HH.text "Save"]
                    , HH.button [ HP.classes [ B.btn, B.btnSecondary ], HE.onClick (\e -> Just $ BackToListView (MouseEvent.toEvent e)) ] [ HH.text "Cancel"]
                    ]
                  else HH.span [] []
                ]
              ]

  handleAction :: Action -> ActionHandler
  handleAction action =  do
     case action of
          IsbnChanged value -> updateBook (\b -> b { isbn = value })
          TitleChanged value -> updateBook (\b -> b { title = value })
          AuthorChanged value -> updateBook (\b -> b { author = value })
          SaveNewBook ev -> do
            H.liftEffect $ preventDefault ev 
            maybeSaveBook
          BackToListView ev -> do 
            H.liftEffect $ preventDefault ev 
            H.raise $ NavigateToRoute BooksIndex

updateBook :: (Book -> Book) -> ActionHandler
updateBook fn = do
  state@{ book } <- H.get
  H.put state { book = fn book }

maybeSaveBook :: ActionHandler
maybeSaveBook = do
  state@{ book } <- H.get
  let validation = validateBook book 
  if Map.isEmpty validation then saveBook 
    else H.put state { validation = validation, message = warningMessage "There are some changes needed to save this new book" }

saveBook :: ActionHandler
saveBook = do
  state@{ book } <- H.get
  _ <- H.put state { posting = true }
  response <- H.liftAff $ AX.request $ (AX.defaultRequest 
               { url = "/api/books"
               , method = Left POST
               , headers = [ ContentType $ MediaType "application/json" ]
               , content = Just $ AXRequest.string $ writeJSON book
               , responseFormat = AXResponse.string  
               })
  case response.status of
    StatusCode 204 -> do
       H.raise $ NavigateToRoute BooksIndex
    _ ->  
      H.put $ state { posting = false, message = warningMessage $ either (\_ -> "Unknown Error") identity response.body  }
