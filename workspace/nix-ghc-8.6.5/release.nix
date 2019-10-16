{ localeLang ? "C.UTF-8" }:
import ../../nix/release/ghc-8.6.5.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}
