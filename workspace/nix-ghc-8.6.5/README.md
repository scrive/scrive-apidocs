# Kontrakcja Nix for GHC 8.6.5

This is the workspace directory for developing and releasing Kontrakcja
using Nix with GHC 8.6.5. This directory is located at
workspace/nix-ghc-8.6.5 from the root of Kontrakcja.

## Why Workspace

There are a few reasons why Nix development is done in a workspace
directory instead of the root directory:

  - The Nix build have its own Cabal dependency versions built as
    Nix packages. The versions are incompatible with the root
    `cabal.project.freeze` file. So running `cabal build` using
    Nix shell in the root directory will cause all cabal packages
    to be rebuilt instead of making use of what's alread in Nix.

  - We can experiment with different versions of dependencies such
    as GHC before officially supporting it. For example the
    GHC 8.8 workspace is currently still in development,
    but when it's ready we can still use GHC 8.8 and 8.6 at the
    same time through Nix.

Because of this, make sure you are building kontrakcja from the
workspace directory and not from the project root!

## Prerequisite

The Nix build have good support on Linux, with some issues that
need to be fixed manually on OS X.

Before running, you need to install Docker outside of Nix. This is
the only dependency not managed by Nix, because Docker installation
requires special setup and root permission.

## Enter Nix Shell

From the project root:

```bash
./nix-shell.sh
```

or

```
cd workspace/nix-ghc-8.6.5
nix-shell
```

## Build Kontrakcja

At any time within Nix shell:

```bash
# If you are not already in workspace
cd $KONTRAKCJA_WORKSPACE
./shake.sh all
```

## Configuration

Before using the workspace, there are a few local configurations you
should place in this workspace directory.

### Locale

The default locale within Nix shell is `en_US.UTF-8`, but this is not
universally supported on all OS. If your locale is different, create
a `.locale` file in the workspace directory with the custom locale
on your OS.

### PDF Tools Lambda

The default generated `kontrakcja.conf` uses Docker container for editing
PDFs, which can be very slow. Get from someone the credentials for the
AWS Lambda for Kontrakcja development, and place it in the workspace directory
as `pdftools-lambda.local.json` with something like:

```json
{
  "gateway_url": "https://xxxxx.execute-api.eu-west-1.amazonaws.com/local/kontra_local",
  "api_key": "xxxxxx",
  "global_sign": {
    "api_key": "xxxxxx",
    "api_password": "xxxxxx",
    "certificate": "xxxxxx",
    "certificate_password": "xxxxxx"
  },
  "amazon_s3": {
    "bucket": "xxxxxxxx",
    "access_key": "xxxxxxx",
    "secret_key": "xxxxxxx"
  }
}
```


## Bootstrap Workspace

Make sure the workspace is first configured following the previous section.

From the workspace directory:

```bash
./init-workspace.sh
```

This should generate the configuration files and initialize the database.

## Run Kontrakcja

At any time within Nix shell:

```bash
# If you are not already in workspace
cd $KONTRAKCJA_WORKSPACE
./run-dev.sh
```

This will run all services related to Kontrakcja, and you can visit
http://localhost:8000 to use the app.

## Run Services Only

You can run only external services such as Postgres and fake S3,
and manually run kontrakcja-server in another shell.

```bash
cd $KONTRAKCJA_WORKSPACE
../scripts/run-services.sh
```

## Front End Development

Nix shell provides CLI tools such as `node`, `npm`, and `grunt` for you.
But for development go to either `frontend/` or `frontend-elm/` from the
project root, and run commands such as `npm run watch` the usual way
from there.

If you are already running `run-dev.sh`, there is no need for this
as it watches the front end directories and rebuild the front end code
as you edit them.

## Nix Build

You can build Kontrakcja as a Nix package. While you are _outside_ of
Nix shell, go to the workspace directory and run:

```
nix-build -A dev-release release.nix
```