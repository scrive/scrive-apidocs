let
  nixpkgs = import ../modules/nixpkgs.nix;

  source-info = nixpkgs.lib.importJSON ./new-frontend.json;
in
builtins.fetchGit {
  inherit (source-info) url rev;
}
