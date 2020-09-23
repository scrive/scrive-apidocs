let
  version = "7e120085693d7fb6a33d78dbae9e0934e043f85e";
  sha256 = "1jkjv2gmvw0bh31i695kxm6vnki8zxz5xp59mxz6s8pdw7pw31nj";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
  }
