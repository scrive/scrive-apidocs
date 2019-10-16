{ localeLang ? "C.UTF-8" }:
import ../../nix/release/ghc-8.4.4.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}
