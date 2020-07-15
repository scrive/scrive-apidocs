{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "5b4b89bdcb622c6b93768f9923290c2ff41ccd91";
  }
