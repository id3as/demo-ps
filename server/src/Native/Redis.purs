module Redis where

import Prelude

import Data.Either (hush)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Erl.Data.List (List, nil)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

newtype DbId = DbId String
derive instance ntDbId :: Newtype DbId _


foreign import data ConnectionString :: Type
foreign import data RedisConnection :: Type
foreign import open :: ConnectionString -> Effect RedisConnection

foreign import delete :: DbId -> RedisConnection -> Effect Unit
foreign import put_ :: DbId -> String -> RedisConnection -> Effect Unit
foreign import get_ :: DbId -> RedisConnection -> (Maybe String) -> (String -> Maybe String) -> Effect (Maybe String)
foreign import readKeyPrefix_ :: String -> RedisConnection -> Effect (List String)

put :: forall a. WriteForeign a => DbId -> a -> RedisConnection -> Effect Unit
put id obj conn = 
  put_ id (writeJSON obj) conn

get :: forall a. ReadForeign a => DbId -> RedisConnection -> Effect (Maybe a)
get id conn = do
  maybeStr <- get_ id conn Nothing Just
  pure $ (hush <<< readJSON) =<< maybeStr
  
findAll :: forall a. ReadForeign a => String -> RedisConnection -> Effect (List a)
findAll prefix conn = do
  items <- readKeyPrefix_ prefix conn
  pure $ filterMap (hush <<< readJSON) items
