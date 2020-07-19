self: super:
  {
    local = (super.local or {}) // {
      erlang_ls = super.beam.packages.erlang.callPackage ./erlang_ls.nix {};
    };
}

