{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "2e2bb3656e2a77b79b7f69aa15540ef5b2cc899d";
  }
