#!/bin/bash
#
# How to do TX synchronisation:
#
# First make sure that you are on good branch - the one that is
# synched with TX. Right now it's staging.
#
# WARNING: If you're on a different branch, you can destroy texts.
#
# First, do:
#
#  ./transifex.sh diff-lang user password en
#
# And check if response looks reasonable. If there are 500 texts
# changed, it's bad, but 50 are ok. Now push your local source texts
# to transifex. This will make all English texts available to
# translators.
#
# ./transifex.sh push-lang user password en
#
# You should never do this with any other language, since it will
# overwrite translations. And translations should be done in TX, not
# in sources.
#
# Now it's time to fetch stuff from TX. You do that with
# ./transifex.sh merge-all user password
#
# This will fetch all source files for all languages. It will include
# English, and it is possible that TX will drop some spaces, etc. It
# still should be fine. For every change it will overwrite local file.
#
# After you merged it is always good to run tests with -t
# Localization. Errors should be fixed in TX, and then you just do
# synchronization again. But at this point you will not need to push
# English texts.
#

if [ $# -lt 1 ]; then
  echo "Transifex integration tool."
  echo "Usage:"
  echo
  echo "transifex.sh fix                                "\
       "-- Sort local .json files with translations."
  echo "transifex.sh push user password lang resource   "\
       "-- Push local language to Transifex. "\
       "It will overwrite the external version."
  echo "transifex.sh diff user password lang resource   "\
       "-- Show a diff between the external version and your local one."
  echo "transifex.sh merge user password lang resource  "\
       "-- Merge the external version with your local one."
  echo

  echo "Available languages : en | sv | de | fr | it | es | pt | "\
       "nl | da | no | fi | is | et | lv | lt"
  echo "Available resources : texts | events | signview"
  exit 0
fi

DIR=`dirname $0`
runhaskell -i"$DIR" "$DIR/Transifex/Synch.hs" $1 $2 $3 $4 $5
