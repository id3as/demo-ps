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
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (label) as HPA
import Halogen.Themes.Bootstrap4 as B
import Routing.Duplex (print)
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)
import Type.Prelude (Proxy(..))
import Web.Event.Event (Event, preventDefault)

type ChildSlots =
  ( bookList :: BookList.Slot Unit
  , bookCreate :: BookCreate.Slot Unit
  , bookEdit :: BookEdit.Slot Unit
  , bookDelete :: BookDelete.Slot Unit
  )

_bookList = Proxy :: Proxy "bookList"
_bookCreate = Proxy :: Proxy "bookCreate"
_bookEdit = Proxy :: Proxy "bookEdit"
_bookDelete = Proxy :: Proxy "bookDelete"

data Query a = Global GlobalMessage a

data Action
  = Initialize
  | Tick
  | RaiseGlobal GlobalMessage
  | SwitchRoute Event Route

type State =
  { activeRoute :: Route
  , nav :: PushStateInterface
  }

component :: H.Component Query PushStateInterface GlobalMessage Aff
component =
  H.mkComponent
    { initialState: (\nav -> { activeRoute: BooksIndex, nav })
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              }
    }
  where

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
              [ HH.nav [ HP.classes [ B.colMd2, B.dNone, B.dBlock, B.bgLight, HH.ClassName "sidebar" ] ]
                  [ HH.div [ HP.classes [ HH.ClassName "sidebar-sticky" ] ]
                      [ HH.ul [ HP.classes [ B.nav, B.flexColumn ] ]
                          [ HH.li [ HP.class_ B.navItem ]
                              [ HH.a
                                  [ HP.classes $ navLinkClass activeRoute BooksIndex
                                  , HP.href (print routeCodec BooksIndex)
                                  ]
                                  [ HH.text "Books" ]
                              ]
                          ]
                      ]
                  ]
              , HH.main [ HP.attr (HH.AttrName "role") "main", HP.classes [ B.colMd9, HH.ClassName "ml-sm-auto", B.colLg10, B.px4 ] ]
                  [ HH.nav [ HPA.label "breadcrumb" ]
                      [ HH.ol [ HP.class_ B.breadcrumb ]
                          $ (HH.li [ HP.class_ B.breadcrumbItem ] [ HH.text "Home" ] : (gatherBreadcrumbs activeRoute))
                      ]
                  , HH.div [ HP.classes [ B.mt2 ] ]
                      [ case activeRoute of
                          Root -> HH.slot _bookList unit BookList.component unit (RaiseGlobal)
                          BooksIndex -> HH.slot _bookList unit BookList.component unit (RaiseGlobal)
                          BooksNew -> HH.slot _bookCreate unit BookCreate.component unit (RaiseGlobal)
                          BooksDelete id -> HH.slot _bookDelete unit BookDelete.component (Isbn id) (RaiseGlobal) -- id
                          BooksEdit id -> HH.slot _bookEdit unit BookEdit.component (Isbn id) (RaiseGlobal)
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
      { nav } <- H.get
      case msg of
        (NavigateToRoute route) ->
          void $ liftEffect $ nav.pushState (write {}) (print routeCodec route)
    SwitchRoute ev page -> do
      H.liftEffect $ preventDefault ev
      H.raise (NavigateToRoute page)

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots GlobalMessage Aff (Maybe a)
  handleQuery = case _ of
    Global (NavigateToRoute route) next -> do
      H.modify_ (_ { activeRoute = route })
      pure $ Just next

gatherBreadcrumbs :: Route -> Array (H.ComponentHTML Action ChildSlots Aff)
gatherBreadcrumbs activeRoute = map (\(Tuple route name) -> HH.li [ HP.class_ B.breadcrumbItem ] [ HH.a [ HP.href (print routeCodec route) ] [ HH.text name ] ]) $ breadcrumbs activeRoute

navLinkClass :: Route -> Route -> Array H.ClassName
navLinkClass current item =
  if current == item then
    [ B.navLink, B.active ]
  else
    [ B.navLink ]
