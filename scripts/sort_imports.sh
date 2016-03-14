#!/bin/bash -e

echo "WARNING: This file will be removed soon"
echo "##teamcity[message text='This file will soon be replaced by Shake, please change the build to not depend on it' status='WARNING']"

runhaskell $(dirname $0)/sort_imports.hs "$@" \
  cron/inc \
  cron/schema \
  cron/src \
  docconv/src \
  docconv/test/src \
  inc \
  mailer/inc \
  mailer/schema \
  mailer/src \
  messenger/inc \
  messenger/schema \
  messenger/src \
  misc \
  pdftools \
  schema/inc \
  schema/src \
  src \
  test/src

echo "WARNING: This file will be removed soon"
echo "##teamcity[message text='This file will soon be replaced by Shake, please change the build to not depend on it' status='WARNING']"
