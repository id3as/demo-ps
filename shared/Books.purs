module Books where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', rest, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

apiRouteCodec :: RouteDuplex' ApiRoute
apiRouteCodec = root (sum
  { "Book": "api" / "books" / segment
  , "BooksIndex": "api" / "books" / noArgs
  , "Assets": "assets" / rest
  , "ClientRoute": rest
  })

data ApiRoute
  = BooksIndex
  | Book String
  | Assets (Array String)
  | ClientRoute (Array String)

derive instance eqRoute :: Eq ApiRoute
derive instance genericRoute :: Generic ApiRoute _

type Book = { isbn :: String
            , title :: String
            , author :: String
            }


