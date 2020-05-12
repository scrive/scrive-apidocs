{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "e4563f04ec0e1ab8aae652adff59a4b55d880b9d";
  }
