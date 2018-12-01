{ nixpkgs ? import <nixpkgs> {}, production ? false }:

with nixpkgs;
with haskell.lib;

let node = import ./node { pkgs = nixpkgs; }; in

(
  (overrideCabal
    (haskell.packages.ghc844.callCabal2nix "xanbot" (builtins.fetchGit ./.) {})
    (drv: {
      configureFlags = lib.optional production [ "-fprod" ];
      buildTools = [ node.shell.drvAttrs.nodeDependencies ];
      preBuild = ''
        ln -sv ${node.shell.drvAttrs.nodeDependencies}/lib/node_modules .
        mkdir -p $webpack
        webpack-cli --mode production --output-path $webpack
      '';
    })
  ).overrideScope (self: super: {
    aeson-flowtyped = dontCheck (doJailbreak super.aeson-flowtyped);
  })
).overrideDerivation (drv: {
  outputs = drv.outputs ++ ["webpack"];
})
