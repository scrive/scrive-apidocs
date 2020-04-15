useLocal: nixOptions :
let
  nixpkgsSource = import ../source/nixpkgs.nix
    { inherit useLocal; };

  override = {
    config = {
      allowBroken = true;
    };
  };
in
import nixpkgsSource (nixOptions // override)
