let
  haskell-nix-src = import ../source/haskell-nix.nix;

  haskell-nix = import haskell-nix-src
    {};
in
haskell-nix
