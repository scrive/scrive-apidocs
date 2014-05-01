#!/usr/bin/perl
#
# teamcity_cabal_sandbox:
#
# 1. cabal update
# 2. cabal sandbox init
# 3. cabal install dependencies
# 4. cabal configure
#
# Publishes artifacts:
# - cabal.sandbox.config
# - dist/setup-config
#
# Trigerred by a change in:
# - kontrakcja.cabal
# - cabal.config
#
# Depends on artifacts: none.
# Depends on configurations: none.
#
# Cabal sandbox should reside outside of source tree so that clean
# build does not remove packages. Cabal sandbox should be named after
# the git branch. This script is triggered if kontrakcja.conf or
# cabal.config change as that might result in different set of
# packages needed or different configuration.

use warnings;
use strict;
use File::Path;
use File::Basename;
use lib dirname (__FILE__);
use teamcity_common;

my $sandboxes = $ENV{"HOME"} . "/cabal-sandboxes/";
my $sandbox = $sandboxes . git_current_branch();

File::Path::make_path($sandbox);

# Update list of packages available at repositories
system_or_die("cabal", "update");

# Initialize sandbox. It is ok to initialize it again if it is there.
system_or_die("cabal", "sandbox", "init", "--sandbox=$sandbox");

# Install dependencies into sandbox. If set of dependencies has
# changed it will install new set.
#
# TODO: Consider colorizing output.
system_or_die("cabal", "install",
              "--only-dependencies",
              "--enable-split-objs",
              "--force-reinstalls");

# We might want these flags somewhere, although I'm not sure about it.
#
#              "--enable-library-coverage",
#              "--enable-library-profiling",

system_or_die("cabal", "configure",
              "--enable-benchmarks",
              "--enable-executable-profiling",
              "--enable-tests");
