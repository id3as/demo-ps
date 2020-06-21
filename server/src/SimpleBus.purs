module SimpleBus where

import Prelude
import Gproc as Gproc
import Pinto (ServerName(..))
import Pinto.Gen as Gen
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap, class  Newtype)

foreign import mapBusMessage :: forall msg recvMsg. BusName -> (msg -> recvMsg) -> (recvMsg -> Maybe recvMsg) -> Maybe recvMsg -> msg -> Maybe recvMsg

newtype BusName = BusName String
derive instance ntBusName :: Newtype BusName _

data Bus msg = Bus BusName

bus :: forall msg. String -> Bus msg
bus name  = Bus $ BusName name

raise :: forall msg. Bus msg ->  msg -> Effect Unit
raise (Bus name) msg =
  Gproc.send (unwrap name) msg

genSubscribe :: forall state msg recvMsg. ServerName state recvMsg -> Bus msg -> (msg -> recvMsg)  -> Effect Unit
genSubscribe serverName (Bus name) lift = do
  _ <- Gen.registerExternalMapping serverName $ mapBusMessage name lift Just Nothing
  _ <- Gproc.reg (unwrap name)
  pure unit
