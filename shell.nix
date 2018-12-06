{ nixpkgs ? import <nixpkgs> {} }@args:
with nixpkgs;

let jse = haskell.lib.justStaticExecutables; in

stdenv.mkDerivation {
  name = "shell-env";
  buildInputs = [
    (jse haskellPackages.cabal-install) (jse haskellPackages.hoogle) stack zlib libiconv
    nodejs ncurses perl
  ];
}
