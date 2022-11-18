
let
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "7c3d4d3af8e9319ccd2a74c31cf247b0fcd08bc2";
    };

  erlangReleases =
    builtins.fetchGit {
      name = "nixpkgs-nixerl";
      url = "https://github.com/id3as/nixpkgs-nixerl.git";
      rev = "9c74cc241f7a13e2ea8ebe765cb3959501c5c404";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "7eadeb83eb2590039c96386d572db3a2fce19370";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
      ];
    };

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      ## not merged yet for 0.15.3 https://github.com/justinwoo/easy-purescript-nix/pull/210
      owner = "toastal";
      repo = "easy-purescript-nix";
      rev = "ed00265f53ae3383a344ce642d40085601420455";
      sha256 = "sha256-X47A46YVcOFTsmi2lFr3yo7EaufBd4ufrTR+ZlPoYz0=";
    }) { pkgs = nixpkgs; };

  erlang = nixpkgs.nixerl.erlang-25-0;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
      erlang.erlang
      erlang.rebar3
      erlang.erlang-ls

      esbuild

      # Purescript
      easy-ps.purs-0_15_3
      easy-ps.spago
      easy-ps.psa
      easy-ps.purescript-language-server
      easy-ps.purs-tidy
      purerl.purerl-0-0-17



   ];
}
