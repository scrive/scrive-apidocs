let
  localeLang =
    if builtins.pathExists ./.locale
    then builtins.readFile ./.locale
    else "C.UTF-8"
    ;
in
import ../../nix/release/ghc-8.8.1.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}
