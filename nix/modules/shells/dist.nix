{ nixpkgs }:
let
  inherit (nixpkgs) pkgs python3Packages;
  vendor = import ../vendor.nix { inherit nixpkgs; };
in
nixpkgs.mkShell {
  name = "kontrakcja-dist-shell";

  buildInputs = [
    pkgs.jq
    pkgs.netcat
    pkgs.postgresql
    pkgs.glibcLocales

    vendor.fakes3
    python3Packages.supervisor
  ];

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}
