{
  nixpkgs
, inHaskellPackages
, quickBuild ? true
}:
let
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

  logSrc = builtins.fetchGit {
    url = "ssh://git@github.com/scrive/log.git";
    rev = "23c905a0d4acbc5c2271d2f0065d8f72fa7052bf";
    ref = "relax-base-version";
  };

  haskellLib = pkgs.haskell.lib;

  haskellPackages0 = inHaskellPackages.override (old: {
    # Use composeExtensions to prevent Nix from obscurely
    # drop any previous overrides
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        hpqtypes = haskellLib.dontCheck
          ( callGitPackage super
            "hpqtypes"
            "ssh://git@github.com/scrive/hpqtypes.git"
            "c32fbe1052706815fc1b598c408c525226f4a963"
          )
        ;

        hpqtypes-extras = haskellLib.dontCheck
          (haskellLib.dontHaddock
            (callGitPackage super
              "hpqtypes-extras"
              "ssh://git@github.com/scrive/hpqtypes-extras.git"
              "25116ca501f9949852271f8905d1fd1dc26a3649"
            ))
        ;

        consumers = pkgs.haskell.lib.appendPatch
          (haskellLib.dontCheck
            (callGitPackage super
              "consumers"
              "ssh://git@github.com/scrive/consumers.git"
              "8b1a2cd4642dd910a8116234a82dd2c3ff1e027d"
            ) )
          ../patches/consumers.patch
        ;

        fields-json = callGitPackage super
          "fields-json"
          "ssh://git@github.com/scrive/fields-json.git"
          "c6d850b24e7d58dd24d95e8676d12ce35155dd4d"
        ;

        hsaml2 = pkgs.haskell.lib.appendPatch
          ( callGitPackage super
              "hsaml2"
              "https://github.com/kubek2k/hsaml2.git"
              "a09ab6fc87fe2311e7bfa56e0dd5141edc758fe7"
          )
          ../patches/hsaml2.patch
        ;

        cryptonite = callGitPackage super
          "cryptonite"
          "https://github.com/haskell-crypto/cryptonite.git"
          "7596e2959d58cbd1bf70ce3af57c0a5db4967add"
        ;

        resource-pool = haskellLib.appendPatch
          (
            callGitPackage super
            "resource-pool"
            "https://github.com/bos/pool.git"
            "f4be4fbe253ac2927d62153a79df9b5957c125e2"
          )
          ../patches/resource-pool.patch
        ;

        unjson = haskellLib.appendPatch
          (haskellLib.dontCheck
            (callGitPackage super
              "unjson"
              "ssh://git@github.com/scrive/unjson.git"
              "53015c8fa91566054db7bf09cb6944e812240ca7"
            ) )
          ../patches/unjson.patch
        ;

        crypto-rng = haskellLib.appendPatch
          (haskellLib.dontCheck
            (callGitPackage super
              "crypto-rng"
              "ssh://git@github.com/scrive/crypto-rng.git"
              "60a15b9d3c8a115d2be5067dc2deeb5b345c8118"
            ) )
          ../patches/crypto-rng.patch
        ;

        Cabal = super.callHackage
            "Cabal"
            "3.0.0.0"
            {}
        ;

        test-framework =
          haskellLib.appendPatch
            (haskellLib.dontCheck
              (super.callHackage
                "test-framework"
                "0.8.2.0"
                {}
                )
              )
            ../patches/test-framework.patch
        ;

        kontrakcja-templates = haskellLib.appendPatch
          ( callGitPackage super
              "kontrakcja-templates"
              "https://github.com/scrive/kontrakcja-templates.git"
              "4aebd3ca85c0058f387e5ca90397c753bdad62dd"
          )
          ../patches/kontrakcja-templates.patch
       ;

        log-base = callPackage super "log-base"
          (logSrc + "/log-base");

        log-postgres = callPackage super "log-postgres"
          (logSrc + "/log-postgres");

        log-elasticsearch = callPackage super "log-elasticsearch"
          (logSrc + "/log-elasticsearch");

        mixpanel = haskellLib.appendPatch
          ( callGitPackage super
              "mixpanel"
              "ssh://git@github.com/scrive/mixpanel.git"
              "d6c378d738f936d7f7950ee278d955726c255535"
          )
          ../patches/mixpanel.patch
        ;

        brittany = callGitPackage super
          "brittany"
          "ssh://git@github.com/lspitzner/brittany.git"
          "38f77f6c5e04883dcbda60286ce88e83275009ab"
        ;

        bloodhound = haskellLib.appendPatch
          ( callGitPackage super
              "bloodhound"
              "ssh://git@github.com/bitemyapp/bloodhound.git"
              "4c743e1082b8b5eec53a7155733999441be0efce"
          )
          ../patches/bloodhound.patch
        ;
      });
  });

  # disable optimizations and tests for quick build
  haskellPackages = if quickBuild
    then haskellPackages0.override (old: {
      overrides = pkgs.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (self: super:
          let
              toPackage = name: val:
                if builtins.typeOf val == "set" &&
                    builtins.hasAttr "doCheck" val &&
                    name != "ghc"
                then
                  haskellLib.dontHaddock (
                    haskellLib.dontCheck (
                      haskellLib.disableOptimization (
                        val
                  ) ) )
                else
                  val
              ;
          in
            pkgs.lib.mapAttrs toPackage super
        );
      })
    else haskellPackages0;

in
haskellPackages
