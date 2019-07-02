module BookClient.Shared where

import Prelude

import Affjax (get) as AX
import Affjax.RequestBody (string)
import Affjax.ResponseFormat (string) as AXResponse
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2, runFn9)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (HTML(..), IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Simple.JSON (class ReadForeign, readJSON)
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as MouseEvent

validationFor :: forall a b. Map.Map String String -> String -> String -> H.ComponentHTML a () b
validationFor validation name default =
  case Map.lookup name validation of
       Nothing -> HH.small [ HP.classes [ B.formText, B.textMuted ], HP.id_ $ name <> "-help"] [ HH.text default ]
       Just message -> HH.small [ HP.classes [ B.formText, B.textDanger ], HP.id_ $ name <> "-help"] [ HH.text message ]

type MessageTtl = Int
data MessageLevel = Info | Warning | Error
data StatusMessage = NoMessage | Message MessageLevel String MessageTtl 

derive instance genericStatusMessage :: Generic StatusMessage _
instance showStatusMessage :: Show StatusMessage where
  show = genericShow

derive instance genericMessageLevel :: Generic MessageLevel _
instance showMessageLevel :: Show MessageLevel where
  show = genericShow

updateMessage :: StatusMessage -> StatusMessage
updateMessage NoMessage = NoMessage
updateMessage (Message _ _ 1) = NoMessage
updateMessage (Message level msg n) = Message level msg (n-1)

noMessage :: StatusMessage
noMessage = NoMessage

successMessage :: String -> StatusMessage
successMessage text = Message Info text 5

warningMessage :: String -> StatusMessage
warningMessage text = Message Warning text 5

renderMessage :: forall a b. StatusMessage -> H.ComponentHTML a () b
renderMessage message =
  case message of
      NoMessage -> HH.span [][]
      Message Info text _ -> HH.div [ HP.classes [ B.alert, B.alertSuccess ] ] [ HH.text text ]
      Message Warning text _ -> HH.div [ HP.classes [ B.alert, B.alertWarning ] ] [ HH.text text ]
      Message Error text _ -> HH.div [ HP.classes [ B.alert, B.alertDanger ] ] [ HH.text text ]

onClick :: forall r i. (Event -> i) -> IProp (onClick :: MouseEvent | r) i
onClick ev = 
  HE.onClick (\e -> Just $ ev (MouseEvent.toEvent e))


noneSelected :: forall a b. HTML a b
noneSelected = HH.option [ HP.value "" ] [ HH.text "None Selected" ]

loadItem :: forall a. ReadForeign a => String -> Aff (Either String a)
loadItem uri = do
  response <- AX.get AXResponse.string uri
  case response.body of
     Left err -> pure $ Left "No"
     Right json -> pure $ bimap show identity $ readJSON json

loadList :: forall a. ReadForeign a => String -> Aff (Array a)
loadList uri = do
  response <- AX.get AXResponse.string uri
  pure $ case response.body of
              Left err -> []
              Right json -> parseList json

parseList :: forall a. ReadForeign a => String -> Array a
parseList input =  
  case readJSON input of
       Left err -> []
       Right result -> result

type ValidationMap = Map.Map String String
