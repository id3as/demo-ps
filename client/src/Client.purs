module BookClient.Client where

import Prelude

import BookClient.Main (Query(..))
import BookClient.Main as Main
import BookClient.Navigation (GlobalMessage(..), routeCodec)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect, mkTell)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    nav <- liftEffect makeInterface
    io <- runUI Main.component nav body
    void $ liftEffect $ matchesWith (parse routeCodec)
      ( \old new ->
          when (old /= Just new) do
            launchAff_ do
              _ <- (io.query $ mkTell $ Global $ NavigateToRoute new)
              pure unit
      )
      nav

