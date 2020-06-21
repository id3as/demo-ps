self: super:
  {
    demo_ps = (super.demo_ps or {}) // {
      spago-0-12-1-0 = super.callPackage ./support/spago/0.12.1.0.nix {};
      dhall-json-1-5-0 = super.callPackage ./support/dhall/dhall-json-1-5-0.nix {};
      purescript-0-13-6 = super.callPackage ./support/purescript/0.13.6.nix {};
    };
}

