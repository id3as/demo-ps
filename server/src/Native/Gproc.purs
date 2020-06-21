module Gproc where

import Prelude
import Effect (Effect) 

foreign import reg :: String -> Effect Unit
foreign import send :: forall msg. String -> msg -> Effect Unit
