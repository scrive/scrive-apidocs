let
  release = import ../../nix/release/ghc-8.4.4.nix {
    workspaceRoot = builtins.toPath(./.);
  };
in
release.manual-shell
