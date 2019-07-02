module BookClient.Books.List where

import Prelude

import BookClient.Navigation (GlobalMessage(..), Route(..))
import BookClient.Shared (StatusMessage(..), loadList, onClick)
import Books (Book)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Web.Event.Event (Event, preventDefault)


data Query a

data Action = Load 
  | Tick
  | EditBook Book Event
  | AddBook Event 
  | DeleteBook Book Event

type Slot = H.Slot Query GlobalMessage

type Model = { message :: StatusMessage, books :: Array Book }

type ActionHandler = H.HalogenM Model Action () GlobalMessage Aff Unit
type RenderHandler = H.ComponentHTML Action () Aff

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

  render :: Model -> RenderHandler
  render model = 
    HH.div [] [ HH.button [ HP.title "Add Book"
                          , HP.classes [ B.mb3 ]
                          , onClick AddBook
                          ] [ HH.text "Add Book" ]
              , HH.h5 [] [ HH.text "Library" ]
              , bookList model.books 
              ]

  handleAction :: Action -> ActionHandler
  handleAction action =  do
     case action of
       Load -> 
         loadBooksIntoState
       Tick -> pure unit 
       EditBook book ev -> do
         H.liftEffect $ preventDefault ev 
         H.raise $ NavigateToRoute $ BooksEdit book.isbn
       AddBook ev -> do
         H.liftEffect $ preventDefault ev 
         H.raise $ NavigateToRoute BooksNew
       DeleteBook book ev -> do
         H.liftEffect $ preventDefault ev 
         H.raise $ NavigateToRoute $ BooksDelete book.isbn

loadBooksIntoState :: ActionHandler
loadBooksIntoState = do
  books <- liftAff $ loadList "/api/books"
  H.put { message: NoMessage, books }

bookList :: Array Book -> RenderHandler
bookList books = HH.ul [] $
  map (\book -> HH.li [] [ HH.text book.title
                         , HH.a [ HP.href "#", onClick $ EditBook book ] [ HH.text " Edit" ]
                         , HH.a [ HP.href "#", onClick $ DeleteBook book ] [ HH.text " Delete" ]
                         ]
      ) books
  

    
