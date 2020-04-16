{ useLocal }:
# ../../../kontrakcja-nix
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "f493273d0090a6ea8bd6daee9b79d42071812c23";
  }
