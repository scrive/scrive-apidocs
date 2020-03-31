# ghcid configuration

## Without Nix workspaces

To enable ghcid support, run `ln -s ghcid/.ghci .ghci` and `ln -s
ghcid/.ghcid` in the root project directory. Then run `ghcid` to get
on-the-fly typechecking after each save.

## With Nix workspaces

The above configuration method doesn't work if you use the Nix workspaces
for development. Instead, you can run ghcid using the `ghcid-nix.sh`
wrapper script like so:

    ./nix-shell.sh              # If not already in a nix-shell
    ../../ghcid/ghcid-nix.sh

See https://github.com/ndmitchell/ghcid for more details.
