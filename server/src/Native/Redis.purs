module Redis where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Erl.Data.List (List, nil)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype DbId = DbId String
derive instance ntDbId :: Newtype DbId _

newtype ConnectionString = ConnectionString String
derive instance ntConnectionString :: Newtype ConnectionString _

foreign import data RedisConnection :: Type
foreign import open :: ConnectionString -> Effect RedisConnection

put :: forall a. WriteForeign a => DbId -> a -> RedisConnection -> Effect Unit
put id obj conn = 
  pure unit

get :: forall a. ReadForeign a => DbId -> RedisConnection -> Effect (Maybe a)
get id conn = 
  pure Nothing
  
findAll :: forall a. ReadForeign a => RedisConnection -> Effect (List a)
findAll conn = 
  pure nil
