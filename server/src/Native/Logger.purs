module Logger
       ( emergency1, emergency2, emergency3, emergency4
       , alert1, alert2, alert3, alert4
       , critical1, critical2, critical3, critical4
       , error1, error2, error3, error4
       , warning1, warning2, warning3, warning4
       , notice1, notice2, notice3, notice4
       , info1, info2, info3, info4
       , debug1, debug2, debug3, debug4
       ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (List, (:), nil)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)


foreign import emergency :: forall a. String -> a -> Effect Foreign
foreign import alert     :: forall a. String -> a -> Effect Foreign
foreign import critical  :: forall a. String -> a -> Effect Foreign
foreign import error     :: forall a. String -> a -> Effect Foreign
foreign import warning   :: forall a. String -> a -> Effect Foreign
foreign import notice    :: forall a. String -> a -> Effect Foreign
foreign import info      :: forall a. String -> a -> Effect Foreign
foreign import debug     :: forall a. String -> a -> Effect Foreign

emergency1 :: forall a. String -> a -> Effect Foreign
emergency1 m a1 = emergency m $ a1 : nil

emergency2 :: forall a b. String -> a -> b -> Effect Foreign
emergency2 m a1 a2 = emergency m $ dumb2 a1 a2

emergency3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
emergency3 m a1 a2 a3 = emergency m $ dumb3 a1 a2 a3

emergency4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
emergency4 m a1 a2 a3 a4 = emergency m $ dumb4 a1 a2 a3 a4

alert1 :: forall a. String -> a -> Effect Foreign
alert1 m a1 = alert m $ a1 : nil

alert2 :: forall a b. String -> a -> b -> Effect Foreign
alert2 m a1 a2 = alert m $ dumb2 a1 a2

alert3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
alert3 m a1 a2 a3 = alert m $ dumb3 a1 a2 a3

alert4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
alert4 m a1 a2 a3 a4 = alert m $ dumb4 a1 a2 a3 a4

critical1 :: forall a. String -> a -> Effect Foreign
critical1 m a1 = critical m $ a1 : nil

critical2 :: forall a b. String -> a -> b -> Effect Foreign
critical2 m a1 a2 = critical m $ dumb2 a1 a2

critical3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
critical3 m a1 a2 a3 = critical m $ dumb3 a1 a2 a3

critical4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
critical4 m a1 a2 a3 a4 = critical m $ dumb4 a1 a2 a3 a4

error1 :: forall a. String -> a -> Effect Foreign
error1 m a1 = error m $ a1 : nil

error2 :: forall a b. String -> a -> b -> Effect Foreign
error2 m a1 a2 = error m $ dumb2 a1 a2

error3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
error3 m a1 a2 a3 = error m $ dumb3 a1 a2 a3

error4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
error4 m a1 a2 a3 a4 = error m $ dumb4 a1 a2 a3 a4

warning1 :: forall a. String -> a -> Effect Foreign
warning1 m a1 = warning m $ a1 : nil

warning2 :: forall a b. String -> a -> b -> Effect Foreign
warning2 m a1 a2 = warning m $ dumb2 a1 a2

warning3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
warning3 m a1 a2 a3 = warning m $ dumb3 a1 a2 a3

warning4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
warning4 m a1 a2 a3 a4 = warning m $ dumb4 a1 a2 a3 a4


notice1 :: forall a. String -> a -> Effect Foreign
notice1 m a1 = notice m $ a1 : nil

notice2 :: forall a b. String -> a -> b -> Effect Foreign
notice2 m a1 a2 = notice m $ dumb2 a1 a2

notice3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
notice3 m a1 a2 a3 = notice m $ dumb3 a1 a2 a3

notice4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
notice4 m a1 a2 a3 a4 = notice m $ dumb4 a1 a2 a3 a4


info1 :: forall a. String -> a -> Effect Foreign
info1 m a1 = info m $ a1 : nil

info2 :: forall a b. String -> a -> b -> Effect Foreign
info2 m a1 a2 = info m $ dumb2 a1 a2

info3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
info3 m a1 a2 a3 = info m $ dumb3 a1 a2 a3

info4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
info4 m a1 a2 a3 a4 = info m $ dumb4 a1 a2 a3 a4


debug1 :: forall a. String -> a -> Effect Foreign
debug1 m a1 = debug m $ a1 : nil

debug2 :: forall a b. String -> a -> b -> Effect Foreign
debug2 m a1 a2 = debug m $ dumb2 a1 a2

debug3 :: forall a b c. String -> a -> b -> c -> Effect Foreign
debug3 m a1 a2 a3 = debug m $ dumb3 a1 a2 a3

debug4 :: forall a b c d. String -> a -> b -> c -> d -> Effect Foreign
debug4 m a1 a2 a3 a4 = debug m $ dumb4 a1 a2 a3 a4


dumb2 :: forall a b c. a -> b -> List c 
dumb2 a1 a2 = (unsafeCoerce a1) : (unsafeCoerce a2) : nil

dumb3 :: forall a b c d. a -> b -> c -> List d
dumb3 a1 a2 a3 = (unsafeCoerce a1) : (unsafeCoerce a2) : (unsafeCoerce a3) : nil

dumb4 :: forall a b c d e. a -> b -> c -> d -> List e
dumb4 a1 a2 a3 a4 = (unsafeCoerce a1) : (unsafeCoerce a2) : (unsafeCoerce a3) : (unsafeCoerce a4) : nil

