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
          , version = "ce340ae6efc24bdbc6cc9357d600e1c4c3814744"
          },

      erl-stetson =
          { dependencies = ["erl-atom" , "erl-binary" , "erl-lists" , "erl-maps" , "erl-tuples" , "erl-modules" , "foreign" , "maybe" , "prelude" , "transformers" , "routing-duplex"]
          , repo = "ssh://git@github.com/id3as/purescript-erl-stetson.git"
          , version = "bfa76658a8d984928a4beb359b7d09b802c68386"
          }
      }

let extras = {
      erl-simplebus =
          { dependencies = [ "erl-process" ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-simplebus.git"
          , version = "e785b8ba8ac74568513c15c2302075752b7d9e5d"
          }
  }

in  upstream ⫽ overrides⫽ extras
