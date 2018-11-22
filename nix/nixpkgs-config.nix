{ compiler }:

let
  # Return a set of packages to override in pkgs.haskell.packages.${compiler}.
  # It takes the "recursive" nixpkgs, that is, the one returned *after* the
  # overloading, and the old Haskell packages before overloading.
  makeOverridenPackages = pkgs: superHaskellPackages:
    let
      scriveHackage2src = name: version:
        builtins.fetchTarball
          "http://hackage.scrive.com/package/${name}-${version}.tar.gz";

      # Download the cabal file from Scrive's hackage and generate a Nix
      # derivation with cabal2nix.
      scriveHackage2nix = name: version: superHaskellPackages.haskellSrc2nix {
        name = "${name}-${version}";
        src = scriveHackage2src name version;
      };

      # Generate a derivation for a Haskell package on Scrive's hackage.
      callScriveHackage = name: version: args:
        let
          drv = superHaskellPackages.callPackage (scriveHackage2nix name version) args;
          fixedDrv = pkgs.haskell.lib.overrideSrc drv {
            src = scriveHackage2src name version;
          };
        in
        pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck fixedDrv);

      # Wrapper around callHackage to disable documentation and tests.
      callHackage = name: version: args:
        pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck
          (superHaskellPackages.callHackage name version args));

      # From Scrive's hackage.
      scrivePackages = {
        fields-json = callScriveHackage "fields-json" "0.2.2.4" {};
        kontrakcja-templates = callScriveHackage "kontrakcja-templates" "0.10" {};
        mixpanel = pkgs.haskell.lib.dontHaddock
          (callScriveHackage "mixpanel" "0.1.6" {});
        resource-pool = callScriveHackage "resource-pool" "0.2.3.2.1" {};
        # FIXME: curl is restored later as this results in infinite recursion
        # for some reason.
        curl = callScriveHackage "curl" "1.3.8.3" {};
      };

      # Extract versions of kontrakcja's dependencies from cabal.config as the
      # versions from Nixpkgs don't necessarily match and we want to avoid
      # recompiling them twice.
      cabalConfig = builtins.fromJSON (builtins.readFile (
        pkgs.runCommand "cabal-config-json"
          { buildInputs = with pkgs; [python36 python36Packages.pyyaml]; }
          "python ${./read-cabal-config.py} ${../cabal.config} > $out"
      ));

      # Attribute set of derivations for the packages in cabal.config.
      hackagePackages =
        let
          mkPackage = name: value:
            let
              baseDrv = callHackage name value.version {};
              drv = pkgs.haskell.lib.overrideCabal
                baseDrv (super: {
                  # It is a bit dirty but we add the system library to all
                  # packages even when they don't actually need them.
                  librarySystemDepends = with pkgs; [ icu postgresql zlib ];
                  # Additional flags from cabal.config.
                  configureFlags = (super.configureFlags or []) ++
                    map (f: "-f${f}") (value.flags or []);
                });
            in
            drv;
        in
        builtins.mapAttrs mkPackage cabalConfig;

    # The order is important as it means a package in scrivePackages has
    # priority over the ones in hackagePackages for example.
    in hackagePackages // scrivePackages // {
      # Add packages from Nixpkgs which are not in cabal.config.
      base-noprelude = callHackage "base-noprelude" "4.11.1.0" {};
      hspec = callHackage "hspec" "2.5.8" {};
      hspec-core = callHackage "hspec-core" "2.5.8" {};
      hspec-discover = callHackage "hspec-discover" "2.5.8" {};
      hspec-meta = callHackage "hspec-meta" "2.5.6" {};

      # Make happstack-server work with network-2.8.0.0
      happstack-server = pkgs.haskell.lib.appendPatch
        hackagePackages.happstack-server
        ./patches/happstack-server.cabal.patch;

      # Restore some packages that should not be overwritten because they fail
      # for some obscure reason.
      inherit (superHaskellPackages) curl dataenc zlib;
    };

in {
  # Scrive packages don't have a free license and Nix won't build unfree
  # programs by default.
  allowUnfree = true;

  packageOverrides = pkgs: let selfPkgs = pkgs.pkgs; in rec {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
          overrides = self: super: makeOverridenPackages selfPkgs super;
        };
      };
    };
  };
}
