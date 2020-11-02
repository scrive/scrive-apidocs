let
  nixpkgs = import ../modules/nixpkgs.nix;

  release = import ./all.nix
    { checkMaterialization = false;
    };

  ghc810-plan = release.ghc810.build-plan;
  ghc88-plan = release.ghc88.build-plan;
in
nixpkgs.stdenv.mkDerivation {
  name = "kontrakcja-release-plans";

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out

    cp -r ${release.ghc810.build-plan} $out/${release.ghc810.ghc-version}
    cp -r ${release.ghc88.build-plan} $out/${release.ghc88.ghc-version}
  '';
}
