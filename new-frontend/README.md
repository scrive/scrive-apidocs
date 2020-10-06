# New Frontend

This is a placeholder directory to integrate Kontrakcja with
[new_frontend](https://github.com/scrive/new_frontend).
We use [Nix](../doc/nix.md) to load and build the version of
new frontend specified in
[nix/source/new-frontend.nix](../nix/source/new-frontend.nix)
and automatically setup the linking of new frontend artifacts
when entering `nix-shell`.

The new frontend assets are served by Kontrakcja from
[`new-frontend/dist`](./dist). This is automatically linked to
`$scrive_new_frontend` when entering `dev-shell`. Non-Nix
users may also symlink `new-frontend/dist` to their local copy
of `new_frontend` project.

## Updating New Frontend

The source information of new frontend is saved at
[`nix/source/new-frontend.json`](../nix/source/new-frontend.json).
To update to the latest commit of `new_frontend` master,
simply run `make sync-new-frontend` from Kontrakcja.
