{pkgs, ...}: let
  haskell = pkgs.callPackage ./haskell.nix {};
in
  haskell.callCabal2nix "derive-has-field" ./. {}
