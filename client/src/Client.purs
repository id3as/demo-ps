module BookClient.Client where

import Prelude

import BookClient.Main (Query(..))
import BookClient.Main as Main
import BookClient.Navigation (GlobalMessage(..), routeCodec)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (forkAff, launchAff_)
import Halogen (hoist, liftEffect, mkTell, subscribe)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Duplex (print)
import Routing.PushState (makeInterface, matchesWith)
import Simple.JSON (write)


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    nav <- liftEffect makeInterface
    io <- runUI Main.component nav body
    void $ liftEffect $ matchesWith (parse routeCodec) (\old new ->
      when (old /= Just new) do
        launchAff_ $ io.query $ mkTell $ Global $ NavigateToRoute new)
      nav

