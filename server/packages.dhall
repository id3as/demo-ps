let upstream = https://github.com/purerl/package-sets/releases/download/erl-0.13.6-20200402/packages.dhall sha256:5442e50aa76c20bd60b2770ab41c68bae80f6ec96f2df1cfaea310673de567d1

let overrides =
      { erl-cowboy =
          { dependencies = [ "erl-modules" ]
          , repo = "https://github.com/id3as/purescript-erl-cowboy.git"
          , version = "4ee391f0349c00d92f68e4331425174eb8bdff9e"
          },

      erl-pinto =
          { dependencies = [ "erl-process" ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version = "fdf18b1e8febb9ff0b8a9ada0eff7fd9de9cad6e"
          },

      erl-stetson =
          { dependencies = ["erl-atom" , "erl-binary" , "erl-lists" , "erl-maps" , "erl-tuples" , "erl-modules" , "foreign" , "maybe" , "prelude" , "transformers" , "routing-duplex"]
          , repo = "ssh://git@github.com/id3as/purescript-erl-stetson.git"
          , version = "d584315bb669cd2a6b190e5dbcc193522406f15f"
          }
      }

let extras = {
       erl-simplebus =
          { dependencies =
              [ "erl-process"
              , "effect"
              ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-simplebus.git"
          , version = "499883e219c9d828ad67cb68726c8e8c4335ff7b"
          }
  }

in  upstream ⫽ overrides⫽ extras
