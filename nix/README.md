# Quick guide

After installing Nix on your development machine, you should be able to run
`nix-shell` successfully. This will drop you into a shell in which you can
build the application with the usual `./shake.sh all`.

For help installing Nix, have a look at the
[Nix manual](https://nixos.org/nix/manual/#chap-installation).

There is no Hydra instance (yet?) that can act as a binary cache so it will
have to compile a lot of stuff. This might take a while!

## Terms and concepts

Nix is a package manager and a configuration tool. It is also a programming
language and we store **Nix expressions** in files ending with `.nix`.

An expression can evaluate to a string, an integer, ... but also to an
**(attribute) set**. A set is a heterogenous map. It's the equivalent of a
dictionary in Python or an object in Javascript (minus the methods.)

It can also evaluate down to a **derivation**. A derivation is a value
describing something that needs to be built and how to build it. It contains
references to the source, references to the dependencies, the build script,
etc. It can then be built by a **builder** and the result will sit in the
**Nix store** (usually `/nix/store`.)

# What's that file?

## nix/hydra/

This directory contains files specific to Hydra as whole. In other words, this
is the configuration for Scrive, not for this repository in particular.

* `spec.json` is the entry point. When creating the project in Hydra, the
fields `Declarative spec file` should be set to `nix/hydra/spec.json` and
`Declarative spec input` to Git checkout with this repository. It specifies how
to configure the project.
* `default.nix` is a Nix derivation that builds the jobsets of the project.
At the moment, there is only one jobset called `master` which builds the latest
version of the application from the branch `master`.
* `master.nix` is the Nix derivation for the only jobset at the moment.
It is using the two files `release.nix` at the root of this repository and at
the root of `new-scrive-pdf-tools`.

## nix/nixpkgs-config.nix

The default Nixpkgs is not suitable to build `kontrakcja` and we need to
overload some packages. We need to change some package versions or add brand
new packages (those from hackage.scrive.com). This is where the magic happens.

## release.nix

There should be such a file at the root of every repository that we want to use
with Nix. Currently, it is this repository and `new-scrive-pdf-tools`.

It evaluates to a attribute set containing Nix derivations for each subproject.
For example, for this repository, it returns a set with two keys `kontrakcja`
and `kontrakcja-frontend`.

These files can then be used by other Nix files such as `shell.nix` and
`nix/hydra/master.nix`.

## shell.nix

This is the file used by `nix-shell` to know what needs to be available in the
development environment.

# Notes about Hydra

`nix/hydra/spec.json` contains the configuration for the project. In addition
to setting up Hydra and creating a project `Scrive`, there are still some more
steps:

* Hydra needs to fetch dependencies from `hackage.scrive.com`, thus the Nix
configuration should contain `allowed-uris = https://hackage.scrive.com`.
* Tests need docker which needs to be ran as root so we can't a completely
isolated builds. The Nix configuration should contain `sandbox = false` and
the users used to build the derivations (usually `nixbld{1,...}`) need to have
access to Docker.

Here is a sample config for NixOS:

```
virtualisation.docker.enable = true;
users.groups.docker.members =
  map (i: "nixbld${toString i}") (builtins.genList (x: x+1) 32);

services.nginx = {
  enable = true;
  virtualHosts."hydra.local".locations."/".proxyPass = "http://localhost:3033/";
};

services.hydra = {
  enable = true;
  buildMachinesFiles = [];
  listenHost = "localhost";
  port = 3033;
  useSubstitutes = true;
  hydraURL = "http://hydra.local";
  notificationSender = "noreply@hydra.local";
};

networking.extraHosts = ''
  127.0.0.1 hydra.local
'';

nix.extraOptions = ''
  allowed-uris = http://hackage.scrive.com
  sandbox = false
'';
```

# Future improvements

* A Hydra instance that builds the projects and acts as a binary cache.
* Generate one jobset for each pull request.
* A second jobset for `master` using the latest version of `nixpkgs`.
* Build VM images for candidates already configured with the binary cache.
