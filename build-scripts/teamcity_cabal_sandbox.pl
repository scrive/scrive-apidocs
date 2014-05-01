#!/usr/bin/perl
#
# teamcity_cabal_sandbox:
#
# - update cabal repositories
# - remove cabal package sandbox directory
# - initialize new cabal sandbox
# - install all dependencies into it
#
# Cabal sandbox should reside outside of source tree so that clean
# build does not remove packages. Cabal sandbox should be named after
# the git branch. This script should be rarely needed, only when
# dependencies are to be rebuild from scratch

use warnings;
use strict;
use File::Path;
use File::Basename;
use lib dirname (__FILE__);
use teamcity_common;

my $sandboxes = $ENV{"HOME"} . "/cabal-sandboxes/";
my $sandbox = $sandboxes . git_current_branch();

File::Path::make_path($sandbox);

system_or_die("cabal", "update");


system_or_die("cabal", "sandbox", "init", "--sandbox=$sandbox");


system_or_die("cabal", "install",
              "--only-dependencies",
              "--enable-split-objs",
              "--force-reinstalls");
#
# We might want these flags somewhere, although I'm not sure about it.
#              "--enable-library-coverage",
#              "--enable-library-profiling",

system_or_die("cabal", "configure",
              "--enable-benchmarks",
              "--enable-executable-profiling",
              "--enable-tests");
