{-
-}
{ name = "demo"
, dependencies =
    [ "console"
    , "effect"
    , "erl-cowboy"
    , "erl-lager"
    , "erl-pinto"
    , "erl-stetson"
    , "psci-support"
    , "simple-json"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
