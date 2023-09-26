{pkgs, ...}:
pkgs.mkShell {
  inputsFrom = [
    (import ./derive-has-field.nix pkgs).env
  ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.retrie
    haskellPackages.fourmolu
    haskellPackages.haskell-language-server
    alejandra
  ];
  withHoogle = true;
  LANG = "en_US.utf8";
}
