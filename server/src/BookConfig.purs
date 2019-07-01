module BookConfig where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Redis (ConnectionString(..))

foreign import readString_ :: String -> Effect String
foreign import readInt_ :: String -> Effect Int

connectionString :: Effect ConnectionString
connectionString =
  wrap <$> readString_ "connection_string" 

webPort :: Effect Int
webPort =
  readInt_ "web_port" 
