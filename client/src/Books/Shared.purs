module BookClient.Books.Shared where

import Prelude

import BookClient.Shared (ValidationMap)
import Books (Book)
import Data.Map (fromFoldable, unions) as Map
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

validateBook :: Book -> ValidationMap
validateBook book = 
  Map.unions [ if book.title == "" then Map.fromFoldable [ Tuple "title" "Title is required" ] else mempty
             , if book.isbn == "" then Map.fromFoldable [ Tuple "isbn" "Isbn is required" ] else mempty
             , if book.author == "" then Map.fromFoldable [ Tuple "author" "Author is required" ] else mempty
  ]

newtype BookInput = Isbn String
derive instance ntBookInput :: Newtype BookInput _
