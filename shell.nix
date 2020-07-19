
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.4-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "9b195c5369b906825134aafce59744e89b85bd37";
    };

  pursPackages =
    builtins.fetchGit {
      name = "purerl-packages";
      url = "git@github.com:purerl/nixpkgs-purerl.git";
      rev = "547e2ef774c69d33c7fcb5cd140e50c936681846";
    };

  supportPackages =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "git@github.com:id3as/nixpkgs-purerl-support.git";
      rev = "c9a9140db5112e74030763292dc93de25adb3482";
    };


  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import pursPackages)
        (import supportPackages)
      ];
    };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
    nodePackages.purescript-language-server

    nixerl.erlang-22-3.erlang
    nixerl.erlang-22-3.rebar3

    purerl.purerl-0-0-6

    purerl-support.erlang_ls-0-4-1
    purerl-support.purescript-0-13-6
    purerl-support.spago-0-12-1-0
    purerl-support.dhall-json-1-5-0
   ];
}
