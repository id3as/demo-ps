module BookClient.Client where

import Prelude

import BookClient.Main as Main
import BookClient.Navigation (GlobalMessage(..), routeCodec)
import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (forkAff)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (print)
import Routing.PushState (makeInterface)
import Simple.JSON (write)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI Main.component unit body
  nav <- liftEffect makeInterface
  _ <- forkAff $ Main.routeSignal nav driver
  driver.subscribe $ CR.consumer \msg -> 
                                  case msg of
                                       NavigateToRoute route -> do
                                         liftEffect $ nav.pushState (write {}) (print routeCodec route)
                                         $> Nothing
