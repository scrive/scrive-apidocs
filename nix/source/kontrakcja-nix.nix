{ useLocal }:
# ../../../kontrakcja-nix
if useLocal
then ../../../kontrakcja-nix
else
  builtins.fetchGit {
    url = "git@github.com:scrive/kontrakcja-nix.git";
    rev = "723d9a10f374c8f9fdfa685d935567b7f82ec3f7";
  }
