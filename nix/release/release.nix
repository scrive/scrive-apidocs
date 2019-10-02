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
    inherit nixpkgs haskellPackages workspaceRoot;
  };
in
{
  inherit
    haskellPackages
    kontrakcja-base
    manual-shell
    cabal-shell
    dev-shell
    dev-release
    production-shell
    production-release;
}
