{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "ab9d6182e57f702d1bc07bf674c16c4094b1ec8c";
  }
