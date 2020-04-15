{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "58dda4b4b448c584f8605c6d71e9c70a4f32f3d3";
  }
