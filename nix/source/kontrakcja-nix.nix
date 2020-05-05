{ useLocal }:
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "6e0bc1a6f7a02a6de38a5aad12dd45c3fb0af2cc";
  }
