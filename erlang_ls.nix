{ stdenv, fetchFromGitHub,
  fetchHex, erlang, rebar3,
  buildRebar3, fetchRebar3Deps,
  tree }:

let
  version = "0.4.1";

  lager = fetchHex {
    pkg = "lager";
    version = "3.8.0";
    sha256 = "1dmvd1zd0mdgpr1hvhga0snsk9anfavnwa4d1mrn1awfd0dm9jzn";
  };

  katana_code = fetchHex {
    pkg = "katana_code";
    version = "0.2.1";
    sha256 = "14p9yq80nrl6slbcqig3fmjm1p8v37vm1rlblac4z0fraqzssj44";
  };

  ranch = fetchHex {
    pkg = "ranch";
    version = "1.7.1";
    sha256 = "1my7mz3x7a1fmjyin55nn1fr2d2rl3y64qf3kpcidxvxg0kqa7a5";
  };

  jsx = fetchHex {
    pkg = "jsx";
    version = "2.9.0";
    sha256 = "1p6q3jvpwpdbgpnfjnbvniacxa62hypam9kn4y55gpdgmcfdpqcf";
  };

  getopt = fetchHex {
    pkg = "getopt";
    version = "1.0.1";
    sha256 = "174mb46c2qd1f4a7507fng4vvscjh1ds7rykfab5rdnfp61spqak";
  };

  docsh = fetchHex {
    pkg = "docsh";
    version = "0.7.2";
    sha256 = "0d310wakhz9n9xs8bfwm0hnp35q17x8vhrnkqcmhsm07pdhv8zaf";
  };

  ephemeral = fetchHex {
    pkg = "ephemeral";
    version = "2.0.4";
    sha256 = "0l68wvz4wr0r9zkmggqqn102z02nka48x72bzxslb72zyy03saab";
  };

  bucs = fetchHex {
    pkg = "bucs";
    version = "1.0.16";
    sha256 = "0qhdinn1ijpsw58x3c4qx3gfll64wd51dfp33vn7mb80lmr5qspz";
  };

  goldrush = fetchHex {
    pkg = "goldrush";
    version = "0.1.9";
    sha256 = "1ssck5yr7rnrfwzm55pbyi1scgs1sl1xim75h5sj5czwrwl43jwr";
  };

  yamerl = fetchHex {
    pkg = "yamerl";
    version = "0.7.0";
    sha256 = "025ia1marjn27ziqaha8nbi0i4hs7qan56dxhgdkdbg2wa0l8nnb";
  };

  redbug = fetchFromGitHub {
    owner = "massemanet";
    repo = "redbug";
    rev = "60fb6e17c164f153a792985a88ac5c67a2364220";
    sha256 = "1r3ffb8j0qh15vh27lnzvsghvgpyn9khannsv9bykwflw7xbk89j";
  };

  tdiff = fetchHex {
    pkg = "tdiff";
    version = "0.1.2";
    sha256 = "0xbq7p9ii2kp49ms1kylj92ih2jiwvqwimb8jy4aalljz5lf3hp0";
  };

  providers = fetchHex {
    pkg = "providers";
    version = "1.8.1";
    sha256 = "183b9128l4af60rs40agqh6kc6db33j4027ad6jajxn4x6nlamz4";
  };

  quickrand = fetchHex {
    pkg = "quickrand";
    version = "1.8.0";
    sha256 = "1ydnvfpdan65ghhgsrdrlv2gjqxy4db9ablmq9xiykgva7wdrykz";
  };

  uuid = fetchHex {
    pkg = "uuid_erl";
    version = "1.8.0";
    sha256 = "1k79syc6jzbv21yhkvr98ji6ydnj6g76sl89fsdk23rzm4crp988";
  };

  cowlib = fetchHex {
    pkg = "cowlib";
    version = "2.3.0";
    sha256 = "16yqjj5ik9afj3zs8xzfhbm4spf7w6878s790lg18m3kkf1qb5fn";
  };

  rebar3_format = fetchHex {
    pkg = "rebar3_format";
    version = "0.2.1";
    sha256 = "1skfaikjh063kl8xp9hxbqbfiiymi4amrxmihspnsqmxh0m5wkw5";
  };

  zipper = fetchHex {
    pkg = "zipper";
    version = "1.0.1";
    sha256 = "0bbwffypd8hx3n2l55jw18dl2dl02zi0r6iccksis7fcy3hx67va";
  };

  elvis_core = fetchHex {
    pkg = "elvis_core";
    version = "0.6.0";
    sha256 = "0q5277bcwkf4lckgjy5mvn90y5rdmcj13xys32f80cs2qfgwxmch";
  };



in
  buildRebar3 rec {
    inherit version;

  name = "erlang_ls";

  ## This doesn't work because beam deps don't work the same way they used to
  ## none of the beam deps exist in nixos at all (fair enough really!)
  beamDeps = [];

  src = fetchFromGitHub {
    owner  = "erlang-ls";
    repo   = "erlang_ls";
    rev    = "${version}";
    sha256 = "1jp4nrb4ns21jga7ysbqpwpkkmmsz90shcgk8qr4ibi4k0ly98ax";
  };

  buildInputs =  [];

  ## This doesn't work because of the git dep
  ## but if it did, would remove the need for the manual dicking about
  ## checkouts = fetchRebar3Deps {
  ##    inherit name version;
  ##    src = "${src}/rebar.lock";
  ##    sha256 = "f2aa10f99fee7657cc2e4ab45e1d6362d0cc2f1a9bfa75c6542ee84abd326149";
  ## };


  ## These  have to be copies rather than
  ## symlinks  because priv gets written to a lot
 configurePhase = ''
   runHook preConfigure
  # ${erlang}/bin/escript ${rebar3.bootstrapper}

   mkdir -p _checkouts
   cp --no-preserve=mode -R ${lager} _checkouts/lager
   cp --no-preserve=mode -R ${katana_code} _checkouts/katana_code
   cp --no-preserve=mode -R ${ranch} _checkouts/ranch
   cp --no-preserve=mode -R ${jsx} _checkouts/jsx
   cp --no-preserve=mode -R ${getopt} _checkouts/getopt
   cp --no-preserve=mode -R ${docsh} _checkouts/docsh
   cp --no-preserve=mode -R ${ephemeral} _checkouts/ephemeral
   cp --no-preserve=mode -R ${bucs} _checkouts/bucs
   cp --no-preserve=mode -R ${goldrush} _checkouts/goldrush
   cp --no-preserve=mode -R ${yamerl} _checkouts/yamerl
   cp --no-preserve=mode -R ${redbug} _checkouts/redbug
   cp --no-preserve=mode -R ${tdiff} _checkouts/tdiff
   cp --no-preserve=mode -R ${providers} _checkouts/providers
   cp --no-preserve=mode -R ${quickrand} _checkouts/quickrand
   cp --no-preserve=mode -R ${uuid} _checkouts/uuid
   cp --no-preserve=mode -R ${cowlib} _checkouts/cowlib
   cp --no-preserve=mode -R ${rebar3_format} _checkouts/rebar3_format
   cp --no-preserve=mode -R ${zipper}  _checkouts/zipper
   cp --no-preserve=mode -R ${elvis_core} _checkouts/elvis_core

   runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    HOME=. make all
    runHook postBuild
  '';
#
  installPhase = ''
    mkdir -p $out/bin
    cp _build/default/bin/erlang_ls $out/bin/erlang_ls
   '';

  meta = {
    homepage = "https://github.com/erlang-ls/erlang_ls";
    description = "Erlang language server";
  };
}
