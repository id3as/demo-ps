{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "client"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "bifunctors"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foreign"
  , "functions"
  , "halogen"
  , "halogen-bootstrap4"
  , "http-methods"
  , "maybe"
  , "media-types"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
