import ../../nix/release/ghc-8.4.4.nix {
  workspaceRoot = builtins.toPath(./.);
}
