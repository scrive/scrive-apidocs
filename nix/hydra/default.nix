# This derivation builds a description of all the jobsets Hydra needs to build.
{ nixpkgs, ... }:
let
  pkgs = import nixpkgs {};

  nixpkgs-revision = (builtins.replaceStrings ["\n"] [""]
    (builtins.readFile ../nixpkgs-revision));

in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat > $out <<EOF
    {
      "master": {
        "enabled": 1,
        "hidden": false,
        "description": "Latest version from the master branch",
        "nixexprinput": "kontrakcja",
        "nixexprpath": "nix/hydra/master.nix",
        "checkinterval": 30,
        "schedulingshares": 100,
        "enableemail": false,
        "emailoverride": "",
        "keepnr": 3,
        "inputs": {
          "kontrakcja": {
            "type": "git",
            "value": "git@github.com:scrive/kontrakcja.git",
            "emailresponsible": false
          },
          "nixpkgs": {
            "type": "git",
            "value": "git://github.com/NixOS/nixpkgs.git ${nixpkgs-revision}",
            "emailresponsible": false
          },
          "scrivepdftools": {
            "type": "git",
            "value": "git@github.com:scrive/new-scrive-pdf-tools.git",
            "emailresponsible": false
          }
        }
      }
    }
    EOF
  '';
}
