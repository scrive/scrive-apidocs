let
  version = "8dd7d8c7f047219a30004839fb9bc328c04560ca";
  sha256 = "01adnrcs2da2bcgf2nyy4ss2mqqhk7b8n9n8zr06qwhyvf9wj45s";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/input-output-hk/haskell.nix/archive/${version}.tar.gz";
  }
