{
  description = "Derive HasField instances";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      derive-has-field = pkgs.callPackage ./derive-has-field.nix {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
      };
      defaultPackage = derive-has-field;
      packages = flake-utils.lib.flattenTree {
        inherit derive-has-field;
      };
    });
}
