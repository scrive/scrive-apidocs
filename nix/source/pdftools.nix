{ useLocal }:
if useLocal
then ../../../new-scrive-pdf-tools
else
  builtins.fetchGit {
    url = "ssh://git@github.com/scrive/new-scrive-pdf-tools.git";
    rev = "9cc08f31920ce89376e7ac75cf10509b1d44b850";
  }
