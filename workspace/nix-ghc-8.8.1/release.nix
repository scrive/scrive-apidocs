{ localeLang ? "C.UTF-8" }:
import ../../nix/release/ghc-8.8.1.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}
