module Books where

import Prelude
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Isbn = Isbn String

derive newtype instance eqIsbnId :: Eq Isbn
derive newtype instance readIsbnId :: ReadForeign Isbn
derive newtype instance writeIsbnId :: WriteForeign Isbn
derive newtype instance showIsbnId :: Show Isbn
derive instance ntIsbnId :: Newtype Isbn _

type Book = { isbn :: Isbn
            , title :: String
            , author :: String
            }


