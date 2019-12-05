{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "client"
, dependencies =
    [ "aff-coroutines"
    , "affjax"
    , "console"
    , "effect"
    , "filterable"
    , "halogen"
    , "halogen-bootstrap4"
    , "js-timers"
    , "prelude"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    , "web-socket"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
