nixOptions :
let
  nixpkgsSource = import ../derivation/nixpkgs-src.nix;

  override = {
    config = {
      allowBroken = true;
    };
  };
in
import nixpkgsSource (nixOptions // override)
