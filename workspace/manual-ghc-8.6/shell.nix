let
  release = import ../../nix/release/ghc-8.6.nix {
    workspaceRoot = builtins.toPath(./.);
  };
in
release.manual-shell
