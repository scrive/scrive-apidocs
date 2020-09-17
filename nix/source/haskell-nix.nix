let
  version = "b64fd964ab5bafbf42be956a0fa04daf28babd49";
  sha256 = "0a9vif111mi7rsbzdxvbhab6apflzagrjff7w8r089l0aqvq7rpl";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/input-output-hk/haskell.nix/archive/${version}.tar.gz";
  }
