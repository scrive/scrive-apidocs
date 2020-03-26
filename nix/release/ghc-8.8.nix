{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "en_US.UTF-8"
}:
let
  ghcVersion = "ghc883";
  inherit (nixpkgs) pkgs;

  callPackage = self: name: src:
    self.callCabal2nix
      name
      src
      {}
  ;

  callGitPackage = self: name: url: rev:
    callPackage self
        name
        (builtins.fetchGit {
          inherit url rev;
        })
      ;

  haskellPackages-1 = pkgs.haskell.packages.${ghcVersion};
  haskellPackages-2 = haskellPackages-1.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        base-noprelude = super.callHackage
          "base-noprelude"
          "4.13.0.0"
          {}
        ;

        list-tries = pkgs.haskell.lib.appendPatch
          ( super.callHackage
              "list-tries"
              "0.6.6"
              {}
          )
          ../patches/list-tries.patch
        ;

        hslogger = pkgs.haskell.lib.appendPatch
          ( super.callHackage
              "hslogger"
              "1.3.1.0"
              {}
          )
          ../patches/hslogger.patch
        ;

        ekg-core = pkgs.haskell.lib.appendPatch
          ( super.callHackage
              "ekg-core"
              "0.1.1.6"
              {}
          )
          ../patches/ekg-core.patch
        ;

        ekg-statsd = pkgs.haskell.lib.appendPatch
          ( callGitPackage super
              "ekg-statsd"
              "https://github.com/tibbe/ekg-statsd.git"
              "653d0fe2c70f2a8fe60ad10f5e4bfb59ef024d55"
          )
          ../patches/ekg-statsd.patch
        ;

        network-bsd =
          super.callHackage
            "network-bsd"
            "2.8.1.0"
            {}
          ;

        ghc-exactprint =
          super.callHackage
            "ghc-exactprint"
            "0.6.2"
            {}
          ;

        happstack-server = super.callHackage
            "happstack-server"
            "7.6.0"
            {}
        ;

        MissingH = pkgs.haskell.lib.appendPatch
          ( callGitPackage super
              "MissingH"
              "https://github.com/haskell-hvr/missingh.git"
              "a936764af1c674f8217c237223c0759e4e4c0684"
          )
          ../patches/missingh.patch
        ;

        hxt = callPackage super
          "hxt"
          ( builtins.fetchGit {
              url = "https://github.com/UweSchmidt/hxt.git";
              rev = "12b5220c68eea09a789ebe64fcfe37c8f0dfd131";
            } + "/hxt"
          )
        ;
      });
    });
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
  inHaskellPackages = haskellPackages-2;
}
