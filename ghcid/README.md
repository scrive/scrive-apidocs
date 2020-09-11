# ghcid configuration

## Without Nix

To enable ghcid support, run `ln -s ghcid/.ghci .ghci` and `ln -s
ghcid/.ghcid` in the root project directory. Then run `ghcid` to get
on-the-fly typechecking after each save.

## With Nix

The above configuration method doesn't work if you use Nix for development.
Instead, you can run ghcid using the `ghcid-nix.sh` wrapper script like so:

    # In kontrakcja project root
    $ nix-shell --run "./ghcid/ghcid-nix.sh"

See https://github.com/ndmitchell/ghcid for more details.
