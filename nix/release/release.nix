{
  nixpkgs ? import ./nixpkgs.nix
, ghcVersion
, inHaskellPackages ? nixpkgs.pkgs.haskell.packages.${ghcVersion}
, workspaceRoot ? builtins.toPath(../..)
}:
let
  haskellPackages = import ../derivation/kontrakcja-build-deps.nix {
    inherit nixpkgs inHaskellPackages;
  };

  kontrakcja-src = import ../derivation/kontrakcja-src.nix;

  kontrakcja-base = import ../derivation/kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  manual-shell = import ../derivation/kontrakcja-manual-shell.nix {
    inherit nixpkgs workspaceRoot;
    haskellPackages = inHaskellPackages;
  };

  cabal-shell = import ../derivation/kontrakcja-cabal-shell.nix {
    inherit nixpkgs haskellPackages workspaceRoot;
  };

  dev-shell = import ../derivation/kontrakcja-dev-shell.nix {
    inherit nixpkgs haskellPackages workspaceRoot;
  };

  dev-release = import ../derivation/kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages;
  };

  production-shell = import ../derivation/kontrakcja-production-shell.nix {
    inherit nixpkgs haskellPackages workspaceRoot;
  };

  production-release = import ../derivation/kontrakcja-production-release.nix {
    inherit nixpkgs haskellPackages;
  };

  dev-deps = nixpkgs.stdenv.mkDerivation {
    name = "kontrakcja-dev-deps";
    src=kontrakcja-src;
    propagatedBuildInputs = dev-shell.buildInputs;
    configurePhase = "echo configure phase..; env";

    buildPhase = ''
      mkdir -p $out
    '';
    testPhase = "";
    installPhase = "echo install phase";
  };
in
{
  inherit
    haskellPackages
    kontrakcja-base
    kontrakcja-src
    manual-shell
    cabal-shell
    dev-shell
    dev-release
    dev-deps
    production-shell
    production-release;
}
