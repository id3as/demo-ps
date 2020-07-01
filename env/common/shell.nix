
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.4-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
    };

  pursPackages =
    builtins.fetchGit {
      name = "purerl-packages";
      url = "git@github.com:purerl/nixpkgs-purerl.git";
      rev = "5da0a433bcefe607e0bd182b79b220af980a4c78";
    };

  supportPackages =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "git@github.com:id3as/nixpkgs-purerl-support.git";
      rev = "2299658a78f2827e3844084861ae4fa88dcddd8b";
    };


  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import pursPackages)
        (import supportPackages)
      ];
    };

  inherit (nixpkgs.stdenv.lib) optionals;
  inherit (nixpkgs)stdenv;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    nixerl.erlang-22-3.erlang
    nixerl.erlang-22-3.rebar3

    purerl.purerl-0-0-5

    purerl-support.purescript-0-13-6
    purerl-support.spago-0-12-1-0
    purerl-support.dhall-json-1-5-0
   ];
}
