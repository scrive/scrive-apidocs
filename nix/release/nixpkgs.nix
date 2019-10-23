nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "bb8b193bd5f3615dad2c25f3d6464774eed89224";
  };

  override = {
    config = {
      allowBroken = true;
    };
  };
in
import nixpkgsSource (nixOptions // override)
