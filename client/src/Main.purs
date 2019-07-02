module BookClient.Main where

import Prelude

import BookClient.Books.Create as BookCreate
import BookClient.Books.Delete as BookDelete
import BookClient.Books.Edit as BookEdit
import BookClient.Books.List as BookList
import BookClient.Books.Shared (BookInput(..))
import BookClient.Navigation (GlobalMessage(..), Route(..), breadcrumbs, routeCodec)
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (label) as HPA
import Halogen.Themes.Bootstrap4 as B
import Routing.Duplex (parse, print)
import Routing.PushState (PushStateInterface, matchesWith)
import Web.Event.Event (Event, preventDefault)

type ChildSlots =
  ( bookList :: BookList.Slot Unit
  , bookCreate :: BookCreate.Slot Unit
  , bookEdit :: BookEdit.Slot Unit
  , bookDelete :: BookDelete.Slot Unit
  )

_bookList :: SProxy "bookList"
_bookList = SProxy

_bookCreate :: SProxy "bookCreate"
_bookCreate = SProxy

_bookEdit :: SProxy "bookEdit"
_bookEdit = SProxy

_bookDelete :: SProxy "bookDelete"
_bookDelete = SProxy

data Query a
  = Global GlobalMessage a

data Action = 
  Initialize 
  | Tick 
  | RaiseGlobal GlobalMessage 
  | SwitchRoute Event Route 

type State = {
  activeRoute :: Route
  }

component :: H.Component HH.HTML Query Unit GlobalMessage Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      }
    }
  where

  initialState :: State
  initialState = { activeRoute : BooksIndex }

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render ({ activeRoute }) =
    HH.div_ 
    [ HH.nav [ HP.classes [ B.navbar, B.navbarDark, B.fixedTop, B.bgDark, B.p0, B.shadow ] ] 
      [ HH.a [ HP.classes [ B.navbarBrand, B.colSm3, B.colMd2, B.mr0 ], HP.href "#" ] [ HH.text "id3as" ]
      , HH.ul [ HP.classes [ B.navbarNav, B.px3 ] ] 
        [ HH.li [ HP.classes [ B.navItem, B.textNowrap ] ] 
          [ HH.a [ HP.classes [ B.navLink ], HP.href "#" ] [ HH.text "Sign out" ] ]
        ]
      ]
    , HH.div [ HP.class_ B.containerFluid ] 
      [ HH.div [ HP.class_ B.row ]
        [ HH.nav [ HP.classes [ B.colMd2, B.dNone, B.dBlock, B.bgLight, HH.ClassName "sidebar"] ] 
          [ HH.div [ HP.classes [ HH.ClassName "sidebar-sticky" ] ] 
            [ HH.ul [ HP.classes [ B.nav, B.flexColumn ] ] [ 
               HH.li [ HP.class_ B.navItem ] [ HH.a [ HP.classes $ navLinkClass activeRoute BooksIndex
                                              , HP.href (print routeCodec BooksIndex)
                                              ] [ HH.text "Books" ] 
                                            ]
            ]
          ]
        ]
        , HH.main [ HP.attr (HH.AttrName "role") "main", HP.classes [ B.colMd9, HH.ClassName "ml-sm-auto", B.colLg10, B.px4 ] ]
          [ HH.nav [ HPA.label "breadcrumb" ] 
            [ HH.ol [ HP.class_ B.breadcrumb ] $
              (HH.li [ HP.class_ B.breadcrumbItem ] [ HH.text "Home" ] : (gatherBreadcrumbs activeRoute))
            ]
            , HH.div [ HP.classes [ B.mt2 ] ] [
              case activeRoute of
                 Root -> HH.slot _bookList unit BookList.component unit (Just <<< RaiseGlobal)
                 BooksIndex -> HH.slot _bookList unit BookList.component unit (Just <<< RaiseGlobal)
                 BooksNew -> HH.slot _bookCreate unit BookCreate.component unit (Just <<< RaiseGlobal)
                 BooksDelete id -> HH.slot _bookDelete unit BookDelete.component (Isbn id) (Just <<< RaiseGlobal) -- id
                 BooksEdit id -> HH.slot _bookEdit unit BookEdit.component (Isbn id) (Just <<< RaiseGlobal) 
               ]
          ]
        ]
      ]
    ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots GlobalMessage Aff Unit
  handleAction = case _ of
    Initialize -> do
       pure unit
    Tick -> do
      pure unit
    RaiseGlobal msg -> do
       H.raise $ msg
    SwitchRoute ev page -> do
      H.liftEffect $ preventDefault ev
      H.raise (NavigateToRoute page)

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots GlobalMessage Aff (Maybe a)
  handleQuery = case _ of
    Global (NavigateToRoute route) next -> do
      H.modify_ (_ { activeRoute = route })
      pure $ Just next

gatherBreadcrumbs :: Route -> Array (H.ComponentHTML Action ChildSlots Aff)
gatherBreadcrumbs activeRoute = 
  map (\(Tuple route name) -> HH.li [ HP.class_ B.breadcrumbItem ] [ HH.a [ HP.href (print routeCodec route) ] [ HH.text name ] ]) $ breadcrumbs activeRoute


routeSignal :: PushStateInterface -> H.HalogenIO Query GlobalMessage Aff -> Aff (Effect Unit)
routeSignal nav driver = H.liftEffect do
  nav # matchesWith (parse routeCodec) routeChanged
  where
    routeChanged _ newRoute = do
      void $ launchAff $ driver.query <<< H.tell <<< Global <<< NavigateToRoute $ newRoute

navLinkClass :: Route -> Route -> Array H.ClassName
navLinkClass current item =
  if current == item then [ B.navLink, B.active ]
    else [ B.navLink ]

