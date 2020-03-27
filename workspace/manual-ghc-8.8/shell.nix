let
  release = import ../../nix/release/ghc-8.8.nix {
    workspaceRoot = builtins.toPath(./.);
  };
in
release.manual-shell
