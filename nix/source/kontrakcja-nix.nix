{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "124f1efb37a3879604cb721a0c62a12098b41863";
  }
