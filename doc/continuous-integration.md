# Continuous Integration on GitHub Actions

This document describes the new CI architecture for Kontrakcja using
[GitHub Actions](https://docs.github.com/en/actions).

## Overview

### Workflow Files

The GitHub Actions workflow are specified as YAML files in
[.github/workflows](../.github/workflows). These files _must not_ be edited
manually, as they are generated from the
[Dhall](https://docs.dhall-lang.org/tutorials/Language-Tour.html) files in
[ci/workflow](../ci/workflow). The detail for generating the workflow YAML
files are described later in the [Dhall Workflow](#dhall-workflow) section.

### Self Hosted Runners

By default, most GitHub Actions workflows run on
[self hosted runners](https://docs.github.com/en/actions/hosting-your-own-runners)
managed by the Scrive GitHub organization. The workflows can also be configured
to run on the default cloud runners provided by GitHub Actions, although they
can be much more costly.

Using self hosted runners may cause jobs to be queued up to be executed sequentially.
If your build has been queued for a long time, check the
[Actions tab](https://github.com/scrive/kontrakcja/actions) and see other jobs in the
queue. Unfortunately the runner status can only be checked by admins of the Scrive
organization. If none of the jobs are running, ping Slack and ask help from devops.

### Nix CI Shells

Each job is run in specialized Nix shells that load minimal set of Nix dependencies
for that job to run successfully. This helps save the loading time of Nix dependencies
in the case when the workflows are running in the default cloud runners.
The Nix shells are defined in [`nix/modules/shells`](../nix/modules/shells), and
more details are available in the [Nix documentation](nix.md).

## Workflows

Here is an overview of different kinds of workflows and jobs that we have configured
for Kontrakcja.

### GHC 8.8 Backend Tests (Nix)

  - Nix shell: `ghc88.backend-shell`

Builds Kontrakcja and runs `kontrakcja-test` inside the GHC 8.8 Nix `backend-shell`.
Also runs `haddock` to make sure that Haddock documentation can be generated without
error. This is the main variant to be executed on each pull request commit.

### Frontend Tests

  - Nix shell: `ghc88.frontend-shell`

Runs the old frontend Grunt tests inside Nix `frontend-shell`. Executes on each
pull request commit.

### Quick Formatting Tests

  - Nix shell: `ghc88.lint-shell`

Runs the quick version of formatting tests inside Nix `lint-shell`. The formatting
tests are only done on files with diff from master. Executes on each pull request
commit.

The formatting tests include:

  - Brittany formatting
  - Hlint
  - Detect unused localization and templates

If any of the formatting tests failed, an artifact file containing the formatting
patch is generated. You can download the patch file and apply it on your local
machine to skip doing the formatting fix yourself. This is particularly useful
for non-Nix users, where mismatch version of Brittany or Hlint might be installed.

### GHC 8.8 Backend Tests (Manual)

  - Nix shell: `ghc88.manual-backend-shell`

Builds Kontrakcja and run `kontrakcja-test` inside Nix `manual-shell`, with only
non-Haskell dependencies provided by Nix. The first run of `cabal build` will
take a long time for building all cabal dependencies. The subsequent builds
should have the cabal dependencies cached in `~/.cabal`, if the workflow
is configured properly.

This only runs on new commits on `master`, `staging`, and `production`. If you notice
that the cabal dependencies are taking long time to rebuild every time the manual
test is run, ping Soares or someone on Slack to fix the cabal cache.

### GHC 8.6 Backend Tests (Nix)

  - Nix shell: `ghc86.backend-shell`

Builds Kontrakcja and runs `kontrakcja-test` inside the GHC 8.6 Nix `backend-shell`.
Although we do not really use GHC 8.6 anymore, this test helps force us to modularize
the CI infrastructure so that it can run on multiple versions of GHC. This is in
preparation for easier migration to GHC 8.10 and GHC 9 in the future.

This only runs on new commits on `master`, `staging`, and `production`.

### Formatting Tests

  - Nix shell: `ghc88.lint-shell`

Runs the full version of formatting tests on all source files. This takes much longer
time to run than the quick version. The test only runs on new commits on
`master`, `staging`, and `production`.

### Selenium Tests

  - Nix shell: `ghc88.selenium-shell`

Run the Selenium tests against deployed version of Kontrakcja, such as dev.scrive.com
and staging.scrive.com. This uses the Nix `selenium-shell` with browser and Python
dependencies installed.

This only runs on new commits on `master`, `staging`, and `production`. Note that
although the Selenium tests are triggered by the commits, they are _not_ tested
against the current source code, rather on what is already deployed on the respective
web addresses.

In future work we may want to change the Selenium tests to be triggered once nightly
using the
[cron syntax](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions#onschedule)
provided by GitHub Actions.

### Cache Nix Dependencies

When the Nix dependencies are updated, this will push the local dependencies to Cachix.
The build is triggered by new commits on `master` and also a special `nix` branch,
when any `.nix` or `.cabal` file has been modified.

The Nix dependencies are built in two stages. In the first stage, it checks out the
Kontrakcja source code and runs `nix-build` on
[`nix/releases/nix-deps.nix`](../nix/releases/nix-deps.nix). This makes a copy of the
Nix expressions of Kontrakcja excluding the source code. The Nix expressions are then
uploaded as an artifact for the second stage.

The second stage always run on a clean cloud runner. It downloads the Nix expression
artifact from the first stage and does not check out any Kontrakcja source code.
It then loads all the Nix shells to force the Nix dependencies to be build.
Finally it uses the Cachix action to push any new Nix dependencies to Cachix.

To make sure that all Nix dependencies are cached before merged, it is best
to push to the `nix` branch to trigger the `nix-deps` job to run. The only purpose
of the `nix` branch is to trigger such jobs, and it is also safe to force push to it
when necessary.

A Mac version of the job, [`nix-deps-mac`]((../nix/releases/nix-deps-mac.nix))
is also defined to build Nix dependencies for Mac. The build runs on the cloud
Mac instances of GitHub Actions. As Mac runners are 10x more expensive,
we only make it build the dependencies for the GHC 8.8 Nix and manual shells.
The Mac build also requires explicit push to `nix` branch to be triggered.
It is not triggered on `master` to avoid unnecessary but costly triggers.

To save cost it is best if we can always remember to build the Mac dependencies
locally on someone’s Mac, push them to Cachix, and then trigger the cloud Mac
build. But it is also good to just leave the option there so that the Mac Nix
cache can still be built even if nobody bothers to build it locally.


### Build Nix Distribution

Builds the GHC 8.8 Nix distribution by running `nix-build -A ghc88.dist release.nix`.
The built Nix artifact is then archived using `nix-store --export` and uploaded as an
artifact named `kontrakcja.nar.gz`. The Nix artifact can then be imported to any
machine with Nix installed, and executables such as `kontrakcja-server` can run
without any additional dependency installed.

The second stage of the workflow downloads the Nix artifact onto a clean cloud
instance, and run the release version of `kontrakcja-test`. This makes sure that
the Nix distribution of Kontrakcja can run without requiring the original
source code.

This only runs on new commits on `master`, `staging`, and `production`.

TODO: the GitHub Actions artifact `kontrakcja.nar.gz` takes about 700~800MB space,
which can accumulate quickly when many build of the Nix distribution is run.
Currently we are cleaning up the old artifacts manually. Unfortunately GitHub
Actions does not support auto cleanup of old artifacts, so we may have to find
other ways to automate the cleanup. Otherwise we may consider disabling the
uploading of the artifact, or only trigger this build manually.

### Build Shake Distribution

  - Nix shell: `ghc88.backend-shell`

Builds the legacy Shake distribution of Kontrakcja using `./shake.sh dist`.
This reads the Nginx configuration specified in [`nginx/`](../nginx).
On success an artifact `kontrakcja.tar.gz` is uploaded.

We should be able to use the `kontrakcja.tar.gz` provided by this job instead
of the TeamCity job for production deployment. However further work is still
needed to verify that the Shake artifact is indeed identical to the one built
on TeamCity.

## Secrets

A number of secrets have been setup in the
[Secrets](https://github.com/scrive/kontrakcja/settings/secrets)
settings of Kontrakcja. Only accounts with admin access to Kontrakcja can
add of modify these secrets. The secrets can be accessed in
[GitHub Actions Workflows](https://docs.github.com/en/actions/configuring-and-managing-workflows/creating-and-storing-encrypted-secrets)
using syntax such as `${{ secrets.CACHIX_AUTH_TOKEN }}`.

  - `SSH_KEY_PDFTOOLS` - The SSH private key with read access to
    [scrive/new-scrive-pdf-tools](https://github.com/scrive/new-scrive-pdf-tools).
    The key can be a deploy key configured in `new-scrive-pdf-tools`.

  - `CACHIX_AUTH_TOKEN` - The auth token with read access to the `scrive` Cachix store.

  - `CACHIX_SIGNING_KEY` - The token with write access to the `scrive` Cachix store.

  - `PDFTOOLS_CONFIG` - The JSON configuration for accessing the AWS Lambda hosted
    PDF tools. This is used by most tests to speed up the run time.

  - `DEV_SELENIUM_CONFIG` - The Python configuration to put in `selenium-test/config.py`.
    This is the configuration for dev.scrive.com

  - `STAGING_SELENIUM_CONFIG` - The Python configuration to put in `selenium-test/config.py`.
    This is the configuration for staging.scrive.com

## Dhall Workflow

GitHub Actions workflows are specified in the YAML DSL. However YAML does not
have good for code reusability, so we instead write the workflow code in
[Dhall](https://docs.dhall-lang.org/tutorials/Language-Tour.html)
and then use `dhall-to-yaml` to convert it into YAML files.

### Workflow Generation

Each `.yaml` file in [.github/workflows/](../.github/workflows) should have a
corresponding `.dhall` file in [ci/workflow/jobs/](../ci/workflow/jobs).
When the Dhall workflow code is modified, the workflow YAML files can be
regenerated using
[`ci/workflow/scripts/generate-workflow.sh`](ci/workflow/scripts/generate-workflow.sh).
When workflow files are added or removed, `generate-workflow.sh` should also
be modified to add or remove the corresponding workflow entry.

### Workflow Configuration

There are a few global configuration parameters available in
[ci/workflow/config.dhall](../ci/workflow/config.dhall) to modify all
workflows at once. This is mainly to make it easy to change how we want to
run the workflows. The config is an object with the following attributes:

  - `self-hosted` - Boolean value for whether to use self hosted runners to
    run the workflows. If this is set to `False`, the cloud runners will be used
    instead. This is currently set to true.

  - `nix-collect-garbage` - Boolean value for whether to run `nix-collect-garbage`
    at the beginning of a job. This is to workaround the current limitation of
    our self hosted runners on AWS Fargate, which only have 25GB of storage limit.
    The option can be removed once we move to using ECS clusters with more
    storage available.

  - `trigger-all-jobs-for-prs` -
    A boolean value of whether to trigger all workflows on pull request commits.
    By default this sets to `False`, to minimize the number of jobs run on
    PR commits. This can be temporarily changed to `True` to test all workflows
    against a particular PR prior to merging. An example for this use is when
    the Selenium tests are modified, and we want to make sure that the tests
    are passing before merging. Note that the value *must* be changed back to
    `False` before merge to avoid overloading the CI runners.

### Data Types

We have minimal definitions of the Dhall datatypes that corresponnd to specific
[workflow syntax](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)
that we used in our workflows. The type definitions are _not_ meant to be comprehensive
to cover all possible workflow syntaxes, as that would only complicate the code
too much without much additional benefit.

If there is a need to use a new workflow syntax, feel free to modify the type
definitions accomodate for the new syntax. Since Dhall is a static type language,
it is easy to refactor the Dhall code as the workflows evolve.

### Triggering Workflow in Specific Pull Request

By default, a pull request only triggers a few basic workflow running.
This is to save the compute cost or queue time of running every workflow on
every PR commit.

If your PR is modifying something that may affect other workflows, or if
you want to test whether a workflow still passes with the current PR,
you can temporary enable running the workflow on the PR by modifying
the following line in the relevant workflow Dhall file:

```dhall
--   Uncomment this line to temporary enable running this workflow in PR
-- , pull_request = Some Workflow.BranchSpec.default
```

And then re-run `ci/workflow/scripts/generate-workflow.sh` to re-generate the
GitHub Actions workflow files. This will qualify your pull request to
run the workflow, while other pull requests won't run it as that line
are not in their branch.

The only thing to take note is to comment out that line again before you
merge your pull request. Otherwise you may accidentally enable that
workflow to run on all pull requests!

## Self Hosted Runners

There are several ways to deploy self hosted runners to run Kontrakcja workflows.
Officially this is managed by the devops team, and the runners are deployed as
AWS Fargate containers. For development purpose, we also maintain a reference
self hosted runner at
[scrive/github-actions-runner](https://github.com/scrive/github-actions-runner).

With appropriate permission, the reference runners can be deployed on local
machines to test out the setup of self hosted runners. We try to make the
self hosted runners to have the same Ubuntu environment as the cloud runners,
so that the workflows can be transparently run in both environments.
