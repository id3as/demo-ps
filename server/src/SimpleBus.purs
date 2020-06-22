module SimpleBus where

import Prelude
import Pinto (ServerName(..))
import Pinto.Gen as Gen
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Erl.Process.Raw (Pid)
import Data.Newtype (unwrap, wrap, class  Newtype)
import Stetson.Types (ReceivingStetsonHandler)

foreign import subscribe_ :: forall msg. BusName -> (msg -> Effect Unit) -> Effect SubscriptionRef 
foreign import unsubscribe :: SubscriptionRef -> Effect Unit
foreign import raise_ :: forall msg. BusName -> msg -> Effect Unit 

newtype SubscriptionRef = SubscriptionRef Pid
newtype BusName = BusName String

derive instance ntBusName :: Newtype BusName _

data Bus msg = Bus BusName

bus :: forall msg. String -> Bus msg
bus name  = Bus $ BusName name

raise :: forall msg. Bus msg ->  msg -> Effect Unit
raise (Bus name) msg =
  raise_ name msg

subscribe :: forall msg. Bus msg -> (msg -> Effect Unit) ->  Effect SubscriptionRef
subscribe (Bus name) callback = 
  subscribe_ name callback
