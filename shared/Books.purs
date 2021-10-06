module Books where

import Prelude
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import GenericJSON (writeTaggedSumRep, taggedSumRep)

newtype Isbn
  = Isbn String

derive newtype instance eqIsbnId :: Eq Isbn

derive newtype instance readIsbnId :: ReadForeign Isbn

derive newtype instance writeIsbnId :: WriteForeign Isbn

derive newtype instance showIsbnId :: Show Isbn

derive instance ntIsbnId :: Newtype Isbn _

type Book
  = { isbn :: Isbn
    , title :: String
    , author :: String
    }

data BookEvent
  = BookCreated Isbn
  | BookUpdated Isbn
  | BookDeleted Isbn

--
-- See: https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html
--
derive instance genericBookEvent :: Generic BookEvent _

instance showBookEvent :: Show BookEvent where
  show = genericShow

instance writeForeignBookEvent :: WriteForeign BookEvent where
  writeImpl = writeTaggedSumRep

instance readForeignBookEvent :: ReadForeign BookEvent where
  readImpl = taggedSumRep
