{pkgs, ...}:
# let aeson = pkgs.callPackage ./nix/aeson.nix {};
# in
pkgs.haskell.packages.ghc94.extend (final: prev: {
  # "aeson" =
  #   final.callCabal2nix
  #   "aeson"
  #   aeson
  #   {};
})
