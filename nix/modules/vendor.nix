{ nixpkgs }:
let
  elm2nix = import ../vendor/elm2nix.nix { inherit nixpkgs; };

  fakes3 = import ../vendor/fakes3/default.nix { inherit nixpkgs; };

  sam = import ../vendor/sam.nix { inherit nixpkgs; };
in
{ inherit
    elm2nix
    fakes3
    sam
  ;
}
