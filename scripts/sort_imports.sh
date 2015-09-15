#!/bin/bash -e

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
