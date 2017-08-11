#!/bin/sh

# Attention: execute it from main repo directory.

# Create and link kontrakcja-test and mailing-server tmp dir
# to the one used by kontrakcja-server so we won't have to
# compile things twice.

# NB: doesn't work with new-build.

# Note: use only if you are aware what it does, since
# somewhere in the future it MAY cause some trouble if eg.
# the same files will be configured differently for each
# project with preprocessor flags.

#
# If you want to get behavior of old configure script, just
# create in main repo directory file named 'configure' with
# the following content:
#
#  #!/bin/sh
#  ./scripts/create_symlinks.sh
#  cabal configure "$@"
#
# and make it executable.
#

# Note: creating symlink for mailing-server is disabled for now
# since mailer doesn't share many files with kontrakcja and thus
# having it doesn't provide sny significant speed boost.

mkdir -p ./dist/build/kontrakcja-server/kontrakcja-server-tmp
mkdir -p ./dist/build/kontrakcja-test
#mkdir -p ./dist/build/mailing-server
ln -sf ../kontrakcja-server/kontrakcja-server-tmp \
       ./dist/build/kontrakcja-test/kontrakcja-test-tmp
#ln -sf ../kontrakcja-server/kontrakcja-server-tmp \
#       ./dist/build/mailing-server/mailing-server-tmp
