let
  nixpkgs = import ../modules/nixpkgs.nix;
in
import ../modules/new-frontend.nix {
  inherit nixpkgs;
}
