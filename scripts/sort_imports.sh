#!/bin/bash -e

runhaskell $(dirname $0)/sort_imports.hs \
  cron/inc \
  cron/src \
  docconv/src \
  docconv/test/src \
  inc \
  mailer/inc \
  mailer/src \
  messenger/inc \
  messenger/src \
  misc \
  pdftools \
  src \
  test/src
