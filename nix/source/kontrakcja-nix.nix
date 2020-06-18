{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "14abf8ae3bab9d78a84dc4a7af09b77495b04f2e";
  }
