let
  localeLang =
    if builtins.pathExists ./.locale
    then builtins.readFile ./.locale
    else "C.UTF-8"
    ;
in
import ../../nix/release/ghc-8.6.5.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}
