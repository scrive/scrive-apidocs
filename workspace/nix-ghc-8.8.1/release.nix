import ../../nix/release/ghc-8.8.1.nix {
  workspaceRoot = builtins.toPath(./.);
}
