module BookClient.Navigation where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple(..))
import Routing.Duplex (RouteDuplex', path, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data GlobalMessage = NavigateToRoute Route

data Route
  = Root
  | BooksIndex
  | BooksNew
  | BooksDelete String
  | BooksEdit String

derive instance eqRoute :: Eq Route
derive instance genericRoute :: Generic Route _

type State = {
  activeRoute :: Route
}

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Root" : noArgs
  , "BooksIndex": "books" / noArgs
  , "BooksNew": "books" / "new" / noArgs
  , "BooksDelete": "books" / string segment / "delete"
  , "BooksEdit": "books" / string segment
  }

breadcrumbs :: Route -> Array (Tuple Route String)
breadcrumbs route = 
  case route of
       Root -> [ Tuple Root "Home" ]
       BooksIndex -> [ Tuple BooksIndex "Books" ]
       BooksNew -> [ Tuple BooksIndex "Books", Tuple BooksNew "Create new" ]
       BooksDelete id -> [ Tuple BooksIndex "Books", Tuple (BooksDelete id) $ "Delete Book " <> id ]
       BooksEdit id -> [ Tuple BooksIndex "Books", Tuple (BooksEdit id) $ "Book " <> id ]
