{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with haskell.lib;

(
  (overrideCabal
    (haskell.packages.ghc844.callCabal2nix "xanbot" (builtins.fetchGit ./.) {})
    (drv: {
      preBuild = ''
        echo $static
        exit 1
      '';
    })
  ).overrideScope (self: super: {
    aeson-flowtyped = dontCheck (doJailbreak super.aeson-flowtyped);
  })
).overrideDerivation (drv: {
  outputs = drv.outputs ++ ["static"];
})
