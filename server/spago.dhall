{-
-}
{ name = "demo"
, dependencies =
    [ "console"
    , "effect"
    , "erl-cowboy"
    , "erl-pinto"
    , "erl-stetson"
    , "psci-support"
    , "simple-json"
    , "erl-simplebus"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
