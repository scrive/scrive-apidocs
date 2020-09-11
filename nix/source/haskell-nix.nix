let
  version = "71a0bba53543aa967c9836fb6ef17eb44a0a7e8c";
  sha256 = "08naamxr4hnsmzx176fwp6sisz79m0zlvqqc5phc32ziyga74qiz";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/input-output-hk/haskell.nix/archive/${version}.tar.gz";
  }
