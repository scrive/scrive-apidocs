{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "524ea50b7edf5c8200b40ceaf4ab975103fecf77";
  }
