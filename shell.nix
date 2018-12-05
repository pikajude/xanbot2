{ nixpkgs ? import <nixpkgs> {} }@args:
with nixpkgs;

stdenv.mkDerivation {
  name = "shell-env";
  buildInputs = [
    haskellPackages.cabal-install haskellPackages.hoogle stack zlib libiconv
    nodejs
  ];
}
