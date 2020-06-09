{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "30b8c3066e026167bfaa8084635e11e783d665af";
  }
