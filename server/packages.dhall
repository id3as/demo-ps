let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

let overrides =
  { erl-simplebus = upstream.erl-simplebus //
    { version = "52d374a8a7a0bb13db6a8ac6552c55e4e2da7d9f"
    }
  , erl-test-eunit = upstream.erl-test-eunit //
    { version = "ed31f51d19f1faba764c32bbad00fa7e4ef752d2"
    }
  , exceptions = upstream.exceptions //
    { version = "edef0014db73aa3136dd4ab2290becb29f6fe6c6"
    , repo = "https://github.com/robashton/purescript-exceptions/"
    }
  }
let additions =
      { erl-opentelemetry =
        { dependencies = [ "effect", "erl-lists", "erl-tuples" ]
        , repo = "https://github.com/id3as/purescript-erl-opentelemetry.git"
        , version = "f8842104a08e4d455084778f5e120347f4a28bd4"
        }
       , erl-binary =
        { dependencies = [ "prelude", "erl-lists" ]
        , repo = "https://github.com/id3as/purescript-erl-binary.git"
        , version = "423f1af8437670beab03463b3e9bc0a487f05ba4"
        }
      , erl-kernel =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-process"
          , "erl-tuples"
          , "erl-untagged-union"
          , "foldable-traversable"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "newtype"
          , "partial"
          , "prelude"
          , "record"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-kernel.git"
        , version = "2c1f78a3aa6993e91e342a984c522b87b98bbb2b"
        }
      , erl-gun =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-kernel"
          , "erl-lists"
          , "erl-maps"
          , "erl-process"
          , "erl-ssl"
          , "erl-tuples"
          , "erl-untagged-union"
          , "foreign"
          , "functions"
          , "maybe"
          , "prelude"
          , "record"
          , "simple-json"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/id3as/purescript-erl-gun.git"
        , version = "c25358f9bae80b9a2512a46f91f51438a7f621fc"
        }
      , erl-otp-types =
        { dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-kernel"
          , "foreign"
          , "prelude"
          , "unsafe-reference"
          ]
        , repo = "https://github.com/id3as/purescript-erl-otp-types.git"
        , version = "6470bc379447c406456e8ef1e6a79c80e3c5e8d1"
        }
      , erl-ranch =
        { dependencies =
          [ "convertable-options"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-kernel"
          , "erl-lists"
          , "erl-maps"
          , "erl-otp-types"
          , "erl-process"
          , "erl-ssl"
          , "erl-tuples"
          , "exceptions"
          , "foreign"
          , "maybe"
          , "prelude"
          , "record"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-ranch.git"
        , version = "08a76bd850ba00c3a120c1d149bed07f9fcc165d"
        }
      , erl-ssl =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "maybe"
          , "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-kernel"
          , "erl-tuples"
          , "erl-logger"
          , "erl-otp-types"
          , "foreign"
          , "maybe"
          , "partial"
          , "prelude"
          , "record"
          , "unsafe-reference"
          ]
        , repo = "https://github.com/id3as/purescript-erl-ssl.git"
        , version = "2bd94ce343448406e579425e1b4140a6b6dd7de0"
        }
      , datetime-parsing =
        { dependencies = 
                [  "arrays"
                 , "datetime"
                 , "either"
                 , "enums"
                 , "foldable-traversable"
                 , "integers"
                 , "lists"
                 , "maybe"
                 , "numbers"
                 , "parsing"
                 , "prelude"
                 , "psci-support"
                 , "strings"  
                ]
        , repo = "https://github.com/flounders/purescript-datetime-parsing"
        , version = "10c0a9aecc60a2a5e8cff35bebe45be4dacaa7f8"
        }
      , sequences =
        { dependencies =
          [ "prelude"
          , "unsafe-coerce"
          , "partial"
          , "unfoldable"
          , "lazy"
          , "arrays"
          , "profunctor"
          , "maybe"
          , "tuples"
          , "newtype"
          ]
        , repo = "https://github.com/id3as/purescript-sequences.git"
        , version = "73fdb04afa32be8a3e3d1d37203592118d4307bc"
        }
      , convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
      , erl-cowboy =
        { repo = "https://github.com/purerl/purescript-erl-cowboy.git"
        , dependencies =
          [ "console"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-kernel"
          , "erl-lists"
          , "erl-maps"
          , "erl-modules"
          , "erl-ranch"
          , "erl-tuples"
          , "foreign"
          , "functions"
          , "maybe"
          , "prelude"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , version = "ca4dd4a6432817fbe6ef9ab1814046f6827046cd"
        }
      , unsafe-reference =
        { repo = "https://github.com/purerl/purescript-unsafe-reference.git"
        , dependencies = [ "prelude"  ]
        , version = "464ee74d0c3ef50e7b661c13399697431f4b6251"
        }
      , erl-stetson =
        { repo = "https://github.com/id3as/purescript-erl-stetson.git"
        , dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-maps"
          , "erl-tuples"
          , "erl-modules"
          , "erl-cowboy"
          , "foreign"
          , "maybe"
          , "prelude"
          , "transformers"
          , "routing-duplex"
          ]
        , version = "a0c0bb4b5ad9046dd69c77197dc5dd025883ada2"
        }
      , erl-untagged-union =
        { dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-tuples"
          , "debug"
          , "foreign"
          , "typelevel-prelude"
          , "maybe"
          , "partial"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-untagged-union.git"
        , version = "eb7a10c7930c4b99f1a6bfce767daa814d45dd2b"
        }
      , rationals =
        { repo = "https://github.com/anttih/purescript-rationals.git"
        , dependencies =
          [ "prelude"
          ]
        , version = "c883c972513380ae161d816ed42108acfe8cc8f6"
        }
      , erl-process =
        { repo = "https://github.com/id3as/purescript-erl-process.git"
        , dependencies =
          [ "console"
          , "prelude"
          , "effect"
          ]
        , version = "afbfa4e7a13c0d55609ff144d49982563fada7f5"
        }
      , erl-pinto =
        { repo = "https://github.com/id3as/purescript-erl-pinto.git"
        , dependencies =
          [ "erl-process"
          , "erl-lists"
          , "erl-atom"
          , "erl-kernel"
          , "datetime"
          , "erl-tuples"
          , "erl-modules"
          , "foreign"
          ]
        , version = "0f8bba4e1c220d763c1e1d71cd0a857dd652d79e"
        }
      , erl-nativerefs =
        { repo = "https://github.com/id3as/purescript-erl-nativerefs.git"
        , dependencies =
          [ "prelude"
          , "effect"
          , "erl-tuples"
          ]
        , version = "b90469380821615adf4cb58782ff246f79eec961"
        }
      , these =
        { repo = "https://github.com/purescript-contrib/purescript-these"
        , version = "a98d0a4e80c9a75fa359e1bcabb0230fb99c52fa"
        , dependencies =
          [ "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          ]

        }
      , uri =
        { repo = "https://github.com/purescript-contrib/purescript-uri"
        , version = "041e717f0c5c663b787b532a00ac706e7897e932"
        , dependencies =
          [ "prelude"
          ]
        }
      , quickcheck =
        { repo = "https://github.com/id3as/purescript-quickcheck"
        , version = "694a781d4f441cfb264b9efa368a0441532133af"
        , dependencies =
          [ "prelude"
          , "lcg"
          ]
        }
      , quickcheck-laws =
        { repo = "https://github.com/purescript-contrib/purescript-quickcheck-laws"
        , version = "464597522e5e001adc2619676584871f423b9ea0"
        , dependencies =
          [ "prelude"
          ]
        }
      }

in  upstream // overrides // additions
