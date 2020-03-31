let
  release = import ../../nix/release/ghc-8.10.nix {
    workspaceRoot = builtins.toPath(./.);
  };
in
release.manual-shell
