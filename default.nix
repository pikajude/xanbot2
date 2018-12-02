{ nixpkgs ? import <nixpkgs> {}, production ? false }:

with nixpkgs;

(haskell.packages.ghc844.override { overrides = self: super: {
  monoidal-containers = self.monoidal-containers_0_4_0_0;

  reflex-basic-host = self.callCabal2nix "reflex-basic-host"
    (fetchgit {
      url = "https://github.com/dalaing/reflex-basic-host";
      rev = "cf7efd48855e30c0940897e12ac694b3617d4e56";
      sha256 = "1cw9lbgl10m6w59fjs4xhjql6p8vvr7acf4wsv2bh5xd8p5svbdm";
    })
    {};

  reflex = haskell.lib.dontCheck (self.callCabal2nix "reflex"
    (fetchgit {
      url = "https://github.com/reflex-frp/reflex";
      rev = "c0f3ee401624a6c5014498ef5db900f0389d9e94";
      sha256 = "10xksvbzjikny3shk56b0yaqkyzc8jsgpmxdasjv7vvw5vkvccai";
    })
    {});
}; }).callCabal2nix "xanbot" (builtins.fetchGit ./.) {}

/*
(hp.callCabal2nix "xanbot" (builtins.fetchGit ./.) {
  reflex-basic-host = hp.callCabal2nix "reflex-basic-host"
    (fetchgit {
      url = "https://github.com/dalaing/reflex-basic-host";
      rev = "cf7efd48855e30c0940897e12ac694b3617d4e56";
      sha256 = "1cw9lbgl10m6w59fjs4xhjql6p8vvr7acf4wsv2bh5xd8p5svbdm";
    })
    {};
}).overrideScope (self: super: {
})
*/
