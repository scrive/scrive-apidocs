let
  nixpkgs = import ../modules/nixpkgs.nix;

  release = import ./all.nix
    { checkMaterialization = false;
    };

  ghc88-plan = release.ghc88.build-plan;
  ghc86-plan = release.ghc86.build-plan;
in
nixpkgs.stdenv.mkDerivation {
  name = "kontrakcja-release-plans";

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out

    cp -r ${release.ghc88.build-plan} $out/${release.ghc88.ghc-version}
    cp -r ${release.ghc86.build-plan} $out/${release.ghc86.ghc-version}
  '';
}
