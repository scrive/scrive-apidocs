let
  release = import ../../nix/release/ghc-8.8.2.nix {
    workspaceRoot = builtins.toPath(./.);
  };
in
release.manual-shell
