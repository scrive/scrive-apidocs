{
  nixpkgs
, inHaskellPackages
}:
let
  inherit (nixpkgs) pkgs;

  callPackage = self: name: src:
    self.callCabal2nix
      name
      src
      {}
  ;

  callScrivePackage = self: name: version:
    callPackage self
        name
        (builtins.fetchTarball
          "http://hackage.scrive.com/package/${name}-${version}.tar.gz")
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
    rev = "c30a152ac80357abeff538e61554465601fc2f13";
  };

  haskellLib = pkgs.haskell.lib;

  haskellPackages = inHaskellPackages.override (old: {
    # Use composeExtensions to prevent Nix from obscurely
    # drop any previous overrides
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {

        # Take Scrive Haskell packages directly from GitHub.
        hpqtypes = haskellLib.dontCheck
          (
            callGitPackage super
            "hpqtypes"
            "ssh://git@github.com/scrive/hpqtypes.git"
            "24e7c6067d0b1bfe12e1efab0800990093cd3f0d"
          )
        ;

        hpqtypes-extras = haskellLib.dontCheck
          (haskellLib.dontHaddock
            (callGitPackage super
              "hpqtypes-extras"
              "ssh://git@github.com/scrive/hpqtypes-extras.git"
              "8323faa8267f0756f29269d63e95fd1a636e97f3"
            ))
        ;

        consumers = haskellLib.dontCheck
          (callGitPackage super
            "consumers"
            "ssh://git@github.com/scrive/consumers.git"
            "98df52670ce51a213ac4d9fe1bef3b08976cbe18"
          )
        ;

        fields-json = callScrivePackage super
          "fields-json"
          "0.2.2.4"
        ;

        resource-pool = callScrivePackage super
          "resource-pool"
          "0.2.3.2.1"
        ;

        unjson = haskellLib.dontCheck
          (super.callHackage
            "unjson"
            "0.15.2.1"
            {})
        ;

        kontrakcja-templates = callScrivePackage super
          "kontrakcja-templates"
          "0.10"
        ;

        log-base = callPackage super "log-base"
          (logSrc + "/log-base");

        log-postgres = callPackage super "log-postgres"
          (logSrc + "/log-postgres");

        log-elasticsearch = callPackage super "log-elasticsearch"
          (logSrc + "/log-elasticsearch");

        mixpanel = haskellLib.appendPatch
          (
            callGitPackage super
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

        # bloodhound has not updated their dependencies on http-client
        # and containers major version for ages. Remove this when
        # it is fixed in new releases.
        bloodhound = haskellLib.dontCheck
          (haskellLib.appendPatch
            (callGitPackage super
              "bloodhound"
              "ssh://git@github.com/bitemyapp/bloodhound.git"
              "4c743e1082b8b5eec53a7155733999441be0efce"
            )
            ../patches/bloodhound.cabal.patch
          )
        ;
      });
  });
in
haskellPackages
