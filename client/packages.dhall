let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220624/packages.dhall
        sha256:08989ed9f53e381f879f1b7012ad7684b1ed64d7164c4ad75e306d3210a46c92

in  upstream
  with halogen-bootstrap4 =
    { dependencies = 
    [
      "halogen"
    ]
    , repo = "https://github.com/mschristiansen/purescript-halogen-bootstrap4.git"
    , version = "v0.2.0"
    }
