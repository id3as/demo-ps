module BookConfig where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Redis (ConnectionString(..))

foreign import readString_ :: String -> Effect String
foreign import readInt_ :: String -> Effect Int
foreign import readDirect_ :: forall a. String -> Effect a

connectionString :: Effect ConnectionString
connectionString =
  readDirect_ "connection_string" 

webPort :: Effect Int
webPort =
  readInt_ "web_port" 
