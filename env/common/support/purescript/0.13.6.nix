{ stdenv
, pkgs
, fetchurl
}:

pkgs.purescript.overrideAttrs (old: rec {
  version = "0.13.6";
  src = if stdenv.isDarwin
        then
        fetchurl {
          url = "https://github.com/purescript/purescript/releases/download/v${version}/macos.tar.gz";
          sha256 = "04kwjjrriyizpvhs96jgyx21ppyd1ynblk24i5825ywxlw9hja25";
        }
        else
        fetchurl {
          url = "https://github.com/purescript/purescript/releases/download/v${version}/linux64.tar.gz";
          sha256 = "012znrj32aq96qh1g2hscdvhl3flgihhimiz40agk0dykpksblns";
        };
})
