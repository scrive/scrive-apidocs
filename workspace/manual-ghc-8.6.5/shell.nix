let
  release = import ../../nix/release/ghc-8.6.5.nix {
    workspaceRoot = builtins.toPath(./.);
  };
in
release.manual-shell
