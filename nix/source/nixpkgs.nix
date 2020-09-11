let
  version = "efc5084e51783ba3410e08da49dd09b95c608e19";
  sha256 = "1rvnxvjl0xmpp50bx1wjrjglwhp4l1x6lpirazrdmm7dnr8zdlj1";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
  }
