nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "85f3d86bea70fe0d76a7e3520966c58604f8e5e9";
  };

  override = {
    config = {
      allowBroken = true;
    };
  };
in
import nixpkgsSource (nixOptions // override)
