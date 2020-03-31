nixOptions :
let
  nixpkgsSource = import ../source/nixpkgs.nix;

  override = {
    config = {
      allowBroken = true;
    };
  };
in
import nixpkgsSource (nixOptions // override)
