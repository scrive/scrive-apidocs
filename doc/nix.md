# Nix Build for Kontrakcja

## Table of Contents

  - [Overview](#overview)

  - [Installation](#installation)

  - [Nix Shells](#nix-shells)

  - [GHC Release Attributes](#ghc-release-attributes)

  - [Nix Distribution](#nix-distribution)

  - [Updating Nix Plans](#updating-nix-plans)

  - [Pure Nix Dependencies](#pure-nix-dependencies)

  - [File Structures](#file-structures)

  - [Nix Resources](#nix-resources)


## Overview

This document describes the use of [Nix](https://nixos.org/nix) for developing and
building Kontrakcja. Kontrakcja is a pretty large project with many dependencies,
and the use of Nix helps simplify the management of both Haskell and non-Haskell
dependencies.

On top of Nix, we use
[Haskell.nix](https://input-output-hk.github.io/haskell.nix/)
instead of the built-in Haskell Nix packages to manage the Haskell dependencies.
There are several advantages of using Haskell.nix. In particular,
Haskell.nix can generate the Haskell dependencies based on the
cabal.project.freeze of Kontrakcja, so both Nix and non-Nix builds
are built with the exact same dependencies. Haskell.nix is also
required for Mac build, as there is a notorious linker error
in the default nixpkgs version due to Kontrakcja having too many
dependencies and modules.


## Installation

Nix can easily [installed](https://nixos.org/download.html) on Linux and Mac
with the following command:

```bash
curl -L https://nixos.org/nix/install | sh
```

After installing Nix, you also need to install [Cachix](https://cachix.org/)
to download cached builds of the Nix artifacts instead of building everything
from scratch.

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

We have a private `scrive` Cachix store which you'll need an authentication
token for access. Generate the auth token at https://app.cachix.org/cache/scrive
and then run the following commands:

```bash
cachix auth <cachix-key>
cachix use scrive
cachix use iohk
```

Other than the `scrive` store, we also use the `iohk` store to download
some of the Nix artifacts provided by Haskell.nix, which are developed
by IOHK.

## Nix Shells

After installation, you can start developing on Kontrakcja by entering the
Nix shell:

```bash
nix-shell
```

The first invocation of Nix shell will take quite some time to load. If things
are cached correctly on Cachix, you should see Nix start downloading from
Cachix. If you see random build commands are being executed for a long time,
it might either be because Cachix is not properly setup on your machine, or
that the cache is not populated properly by the CI. in such case ask on
Slack or Jira for help.

By default, Nix downloads each cache in serial which can be quite slow.
For faster downloading, you can provide either `-j4` or `-j8` to have
4 or 8 parallel downloads.

```bash
nix-shell -j4
```

Note that the `-j` option also applies to parallel builds, so you may want to
tune that down if Nix tries to build too many things at the same time.

By default, `nix-shell` uses the shell specified in [`shell.nix`](../shell.nix),
which points to the GHC 8.8 `dev-shell`. The same shell can be accessed
using the qualified `ghc88.dev-shell` attribute in `release.nix`:

```bash
nix-shell -A ghc88.dev-shell release.nix
```

There are other GHC versions available, such as the GHC 8.6 shell
in `ghc86.dev-shell`:

```bash
nix-shell -A ghc86.dev-shell release.nix
```

Right now we only have GHC 8.8 and 8.6 Nix releases. The GHC 8.6 shell
is not really used anymore, but we are keeping it to keep the code
hygenic and compatible for easier upgrade to GHC 8.10 and 9.0 in the
future.

Other than the `dev-shell`, there is also a `manual-shell` that
can be entered as follows:

```bash
nix-shell -A ghc88.manual-shell release.nix
```

The manual shell only provides non-Nix dependencies and vanilla
GHC and cabal-install. This is used to have a native cabal
build of Kontrakcja directly from Hackage. The manual shell
is also used when we want to generate new `cabal.project.freeze`
file when the Haskell dependencies are updated.

## GHC Release Attributes

For each GHC version attribute in the root `release.nix`, e,g. `ghc88` and
`ghc86`, there are a few useful attributes that can be used from the root
project directory.

  - `dev-shell` - The developer shell for developing Kontrakcja.

    ```bash
    nix-shell -A ghc88.dev-shell release.nix
    ```

  - `manual-shell` - The manual shell with only non-Haskell dependencies
    managed by Nix.

    ```bash
    nix-shell -A ghc88.manual-shell release.nix
    ```

  - `dist` - The Nix distribution of Kontrakcja.

    ```bash
    nix-build -A ghc88.dist release.nix
    ```

  - `kontrakcja-project` - The Haskell.nix derivation of Kontrakcja.
    Contains the same set of Haskell.nix project attributes described
    in Haskell.nix's documentation.

    ```bash
    nix-build release.nix -A \
      ghc88.kontrakcja-project.kontrakcja.components.exes.kontrakcja-server

    nix-build release.nix -A \
      ghc88.kontrakcja-project.kontrakcja-shake.components.exes.kontrakcja-shake

    nix-build release.nix -A \
      ghc88.kontrakcja-project.kontrakcja.components.tests.kontrakcja-test
    ```

  - `kontrakcja-frontend` - The frontend distribution of Kontrakcja.
    Builds the front end source code such as JavaScript, Elm, SASS, and LESS.

    ```bash
    nix-build -A ghc88.kontrakcja-frontend release.nix
    ```

  - `scrivepdftools` - The version of `scrivepdftools.jar` used by Kontrakcja
    inside Nix. Note that this _may_ not be the same as the JAR file in
    [`scrivepdftools/newscrivepdftools.jar`](../scrivepdftools/newscrivepdftools.jar).

    ```bash
    nix-build -A ghc88.scrivepdftools release.nix
    ```

  - `build-plan` - The build plan for Kontrakcja's Haskell.nix dependencies
    that can be copied over to `nix/plans`.

    ```bash
    nix-build -A ghc88.build-plan release.nix
    ```

## Nix Distribution

We have a Nix build recipe for building Kontrakcja itself inside the
`dist` attribute, which can be built using `nix-build` as follows:

```bash
nix-build -A ghc88.dist release.nix
```

Once built, a `result` symlink will be created, pointing to the Nix
artifact containing all Kontrakcja executables. Note that the symlink
is not permanent, and will be overriden when another `nix-build` command
is executed.

You can for example start the Kontrakcja server as follows:

```bash
result/bin/kontrakcja-server
```

You can also run the test suite to make sure that the distribution
version of Kontrakcja is working as expected.


```bash
result/bin/kontrakcja-test
```

The Nix distribution of Kontrakcja does not require the original
source code of Kontrakcja to be present. In fact you can get
the absolute path from the `result` symlink, and execute Kontrakcja
from anywhere. The executables will use the current directory
as the workspace directory. Hence config files such as `kontrakcja.conf`
have to be present in the directory that you invoke the executables.

You can export the Nix artifact as a _Nix Archive_, which has a `.nar`
extension by default:

```bash
nix-store --export $(nix-store -qR result)  > kontrakcja.nar
```

The command above generates a `kontrakcja.nar` from the `result`
symlink earlier. You can the copy this file to another server
and import it as follows:

```bash
kontrakcja=$(cat kontrakcja.nar | nix-store --import)
```

This will import the Nix artifacts from `kontrakcja.nar`, and the
result path is then set in `$kontrakcja`. You can then run the executables
such as follows:

```bash
$kontrakcja/bin/kontrakcja-server
```

The target machine only needs to have Nix installed and nothing else.
Multiple versions of Kontrakcja can be imported this way without conflict,
since each version have a different absolute path in `/nix`.
This can significantly simplifies the deployment process if we choose
to use this approach for production in the future.

## Updating Nix Plans

Haskell.nix encodes the cabal dependencies specified in `cabal.project.freeze`
into _plan_ Nix files that are stored in [nix/plans](../nix/plans).
These files are generated and have to be updated every time the Haskell
dependencies are updated. Failing to do so may result in failed cabal builds
inside the Nix shell, caused by the new Haskell dependencies not being present.

A script [`sync-plan.sh`](../nix/scripts/sync-plan.sh) is provided to
ease the generation of the plan Nix files. Simply execute the follows:

```bash
./nix/scripts/sync-plan.sh
```

and check in the file changes in `nix/plans` into git.

### cabal-deps.project

Haskell.nix requires all non-Hackage dependencies to be specified in the
`cabal.project` file. Furthermore it is not easy to support custom Hackage
repositories such as `hackage.scrive.com` to be used by Haskell.nix.
Because of this, we create a `cabal-deps.project` file specifically
for Haskell.nix to know where to fetch the Scrive Hackage packages
directly from GitHub.

Other than the commit IDs in `source-repository-package`, there are also
`sha256` fields presented as comments for Haskell.nix to statically
determine the SHA256 checksum of the Nix derivations for caching purposes.

When a non-Hackage cabal dependency is updated, it must be updated in
`cabal-deps.project` as well for Nix to continue to work. The `sha256`
field _must_ also be updated to force Nix to use the new version of the
dependency. This can be done by replacing the last few digits of the
original sha256 to some invalid gibberish like `000000`. e.g.:

```
source-repository-package
    type: git
    location: https://github.com/soareschen/mixpanel.git
    tag: d6c378d738f936d7f7950ee278d955726c255535
    --sha256: 13x4g9128bkfyyk0jikxk0jg2ixzr371k123a1swpmal6b000000
```

Then run `nix-shell` and you should get error message such as:

```
hash mismatch in fixed-output derivation '/nix/store/x69dqv2ykh1z9y9p48njynjm0ym6ghc6-mixpanel-d6c378d':
  wanted: sha256:13x4g9128bkfyyk0jikxk0jg2ixzr371k123a1swpmal6b000000
  got:    sha256:1x44ijvibxcnky8ay1mcfmhf56sn49n7w9hb3ag7d20s6yysn4vc
cannot build derivation '/nix/store/93msxrxh5bdsmjndjqv5sg9vnmdzn8fc-mixpanel-lib-mixpanel-0.1.6.3.drv': 1 dependencies couldn't be built
error: build of '/nix/store/1ay249q3s7fgcwdnq70s67kzh1976sm4-ghc-shell-for-packages-ghc-8.8.3-env.drv', '/nix/store/93msxrxh5bdsmjndjqv5sg9vnmdzn8fc-mixpanel-lib-mixpanel-0.1.6.3.drv', '/nix/store/q7wifkz708fgg6b2crllph1frhfgnq18-ghc-shell-for-packages-config.drv' failed
```

Now we can paste the correct SHA256 value
`1x44ijvibxcnky8ay1mcfmhf56sn49n7w9hb3ag7d20s6yysn4vc`
into the `sha256` field.
Following that, you _must_ also run `sync-plan.sh` to update the Nix plan to
use the updated dependency.

### cabal-nix.project

Inside Nix shell, a wrapped version of `cabal-install` is used to always
append the flag `--project-file=cabal-nix.project` to use the
[`cabal-nix.project`](../cabal-nix.project) file instead of
[`cabal.project`](../cabal.project). This prevents cabal from re-downloading
and installing the Nix dependencies specified in the `source-repository-package`
fields in `cabal.project`.

The wrapped cabal also skips calls to `cabal update` as it is not necessary
to update the Hackage index inside the Nix-managed cabal. However if
you want to force `cabal update` to run, you can unset the
`SKIP_CABAL_UPDATE` environment variable which is set inside Nix shell. i.e.:

```bash
SKIP_CABAL_UPDATE=0 cabal update
```

You can also use the original cabal executable that is now renamed to
`cabal-original`:

```bash
cabal-original update
```


## Pure Nix Dependencies

The Nix files in [`nix/`](../nix) are tightly coupled to the Kontrakcja
source code. In other words, it is difficult to run the Nix builds
without involving the Haskell source code. Ideally, we want to have
a pure set of Nix expressions that do not directly depends on the
Kontrakcja source code, so that we can be confident that the Nix
dependencies built do not leak any intellectual property.

This is in particular useful for safely pushing only the Nix dependencies
to Cachix. If our Cachix store does not contain any source code or
builds of Kontrakcja itself, we can less worry about someone having
unauthorized access to the Cachix store, as there is nothing confidential
being stored there.

Fortunately, we can copy a set of Nix files together with the minimal
dependency files needed to evaluate the Nix expressions. The pure Nix
release can be built from
[`nix/releases/nix-deps.nix`](../nix/releases/nix-deps.nix):

```bash
nix-build nix/releases/nix-deps.nix
```

After the run, a `result` symlink is created. The linked directory
contains the main `nix/` directory copied over, together with
some other files such as `kontrakcja.cabal` and other Nix-related
files. Crucially the result directory does not contain any Haskell
source code, other than a few mock `Main.hs` files that are used
to allow the invocation of `cabal configure`. We can for example
copy this result directory into another clean machine, and
run `nix-shell` from it. The Nix shell can then be entered
without needing access to the Kontrakcja source code.
Under such environment, we can safely cache the entire `/nix` directory
with confidentiality intact.

## File Structures

Eventually, the Nix files will have to be maintained by someone who
are new to Nix development. Here is a high level overview of what
each Nix file does.

### [shell.nix](../shell.nix)

This is the default shell that is used when `nix-shell` is executed
with no argument. It simply uses the `ghc88.dev-shell` attribute in
`release.nix`.

### [release.nix](../release.nix)

This is the main entry point for accessing all Nix attributes for
Kontrakcja. It simply links to the actual release file at
[`nix/releases/all.nix`](../nix/releases/all.nix).

### [nix/releases/](../nix/releases)

This is the main directory for all Nix releases of Kontrakcja.
Advanced Nix users should be able to access all Nix attributes
from this directory. There are a few Nix releases available:

  - [all.nix](../nix/releases/all.nix) - The entry point to all
    GHC-specific releases. Currently this contains the `ghc88` and
    `ghc86` attributes, which are imported from
    [ghc88.nix](../nix/releases/ghc88.nix) and
    [ghc86.nix](../nix/releases/ghc86.nix), respectively.

  - [nix-deps.nix](../nix/releases/nix-deps.nix) - The release
    build for pure Nix expressions that does not depend on
    the Kontrakcja source code.

  - [plan.nix](../nix/releases/plan.nix) - The release for generating
    the Haskell.nix build plan which is then copied over to
    [nix/plans](../nix/plans).


### [nix/source/](../nix/source)

This is the directory for the Nix expressions for the loading of various
source code into Nix. When the source code of a dependency is updated,
the relevant file should also be updated to contain the updated
git commit ID and sha256.

  - [`nixpkgs.nix`](../nix/source/nixpkgs.nix) - The pinned version of
    [nixpkgs](https://github.com/NixOS/nixpkgs) to be used.

  - [`haskell-nix.nix`](../nix/source/haskell-nix.nix) - The pinned version
    of [Haskell.nix](https://github.com/input-output-hk/haskell.nix)
    to be used.

  - [`pdftools.nix`](../nix/source/pdftools.nix) - The pinned version of
    [new-scrive-pdf-tools](https://github.com/scrive/new-scrive-pdf-tools)
    to be used inside local AWS SAM. Note that this will produce the
    `scrivepdftools.jar` used by Kontrakcja, which _may_ be different from
    [`scrivepdftools/newscrivepdftools.jar`](../scrivepdftools/newscrivepdftools.jar).
    It is the responsibility of the person updating pdftools to update both
    versions at the same time.

  - [`kontrakcja.nix`](../nix/source/pdftools.nix) - The source code of
    Kontrakcja itself. This is a cleaned git version from the root source
    directory, with local files such as specified in `.gitignore` being
    filtered out. This helps to create reproducible build when developing
    locally.

It is recommended to update the commit ID for nixpkgs and Haskell.nix
frequently, or at least once every few months. This is so that the upstream
cachix from cache.nixos.org and IOHK cachix does not get expired.

To update, first get a relatively new commit ID from the git repository,
and replace the commit ID. Note that you might want to get a commit of
at least a day old for nixpkgs, as otherwise a commit that is too new
might not yet have the cache populated at cache.nixos.org.

Replace the commit ID field with the new commit. At the same time
also _importantly_ change the last few digits of the original sha256 to
some invalid gibberish like `000000`. e.g.:

```nix
let
  version = "dbacb52ad85216c25567978f7f968c8856b5e686";
  sha256 = "1cpcdhx5rwzzppdadd4yr1imgzyrhmscaaaqycrdjn5frd000000";
in ...
```

Try running a Nix command again, and you will get a error message like
follows:

```
$ nix-shell
error: hash mismatch in file downloaded from 'https://github.com/NixOS/nixpkgs/archive/dbacb52ad85216c25567978f7f968c8856b5e686.tar.gz':
  wanted: sha256:1cpcdhx5rwzzppdadd4yr1imgzyrhmscaaaqycrdjn5frd000000
  got:    sha256:1cpcdhx5rwzzppdadd4yr1imgzyrhmscaaaqycrdjn5frdcpl1rl
(use '--show-trace' to show detailed location information)
```

Now you can copy the correct sha256 value
`1cpcdhx5rwzzppdadd4yr1imgzyrhmscaaaqycrdjn5frdcpl1rl`
into the corresponding field.

### [nix/modules/](../nix/modules)

This directory contains the main Nix code that form the various release
Nix expressions. New Nix code should be added to this directory, unless
they need to be directly accessed by external users. The main entry point
to all Nix modules is `release.nix`, which does manual glueing and
dependency injection on Nix modules that depend on each others.

  - [`release.nix`](../nix/modules/release.nix) - The main template
    for generating the GHC-specific releases. It is parameterized by
    attributes such as `ghc-version`, so that they can be instantiated
    with multiple GHC versions. `release.nix` is responsible for glueing
    together all other modules in the directory, and return them as a
    single release dictionary with attributes such as `dev-shell` and
    `dist`.

  - [`kontrakcja-project.nix`](../nix/modules/kontrakcja-project.nix) -
    The Haskell.nix derivation from Kontrakcja's cabal project.

  - [`dist.nix`](../nix/modules/dist.nix) - The Nix distribution of
    Kontrakcja. It builds all Kontrakcja artifacts and copy them over
    into a single Nix derivation, together with static artifacts
    from the Kontrakcja project.

  - [`kontrakcja-frontend.nix`](../nix/modules/kontrakcja-frontend.nix) -
    The Node.js and Elm derivations of the Kontrakcja frontend project.
    The frontend is built without relying on the Haskell source code
    of Kontrakcja. The built Nix artifact can be directly deployed to
    the CDN.

  - [`scrive-pdf-tools.nix`](../nix/modules/scrive-pdf-tools.nix) -
    The Java derivations of scrivepdftools.

  - [`haskell-deps.nix`](../nix/modules/haskell-deps.nix) -
    The other Haskell.nix derivations for Haskell executables used by
    Kontrakcja. This includes `brittany`, `hlint`, `apply-refact`,
    and `cabal-install`.

  - [`run-deps.nix`](../nix/modules/run-deps.nix) -
    The runtime dependencies used when developing Kontrakcja. This
    includes executables such as `postgresql`, `nodejs`, and
    `imagemagick`. If you need a new runtime dependency added to
    Kontrakcja, add it to this file.

  - [`vendor.nix`] - Derivations for third party Nix dependencies such
    as AWS SAM, FakeS3, and Elm2Nix.

  - [`materialized-plan.nix`](../nix/modules/materialized-plan.nix) -
    Loads the haskell.nix plans from [`nix/plans`](../nix/plans)
    to be used by `kontrakcja-project` and `haskell-deps`.

  - [`build-plan.nix`](../nix/modules/build-plan.nix) -
    Template for building the Nix plans for a Haskell.nix project.

  - [`build-plans.nix`](../nix/modules/build-plans.nix) -
    Builds all the Haskell.nix project plans, including
    `kontrakcja-project`, `brittany`, etc.

### [nix/modules/shells/](../nix/modules/shells)

This subdirectory contains all the Nix modules that derive shells that
can be entered using `nix-shell`. Other than `dev.nix` and `manual.nix`,
most other shells contain subset of the dependencies of `dev-shell`
for faster downloading of Nix dependencies when loading the Nix shell
from stateless CI runners such as on GitHub Actions.

  - [`dev.nix`](../nix/modules/shells/dev.nix) - The derivation for `dev-shell`.

  - [`manual.nix`](../nix/modules/shells/manual.nix) - The derivation for
    `manual-shell`.

## Nix Resources

  - [Nix Manual](https://nixos.org/nix/manual/)
  - [Haskell.nix Manual](https://input-output-hk.github.io/haskell.nix/)
  - [Nix.dev](https://nix.dev/)
  - [Nix Pills](https://nixos.org/nixos/nix-pills/)
