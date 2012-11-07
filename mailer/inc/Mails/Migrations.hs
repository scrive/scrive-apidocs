{-# LANGUAGE OverloadedStrings #-}
module Mails.Migrations (
    mailerMigrations
  ) where

import DB
import Mails.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
mailerMigrations :: MonadDB m => [Migration m]
mailerMigrations = [
    addTestServiceToMails
  ]

addTestServiceToMails :: MonadDB m => Migration m
addTestServiceToMails =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE mails ADD COLUMN service_test BOOL"
      _ <- kRun $ SQL "UPDATE mails SET service_test = ?" [toSql False]
      kRunRaw "ALTER TABLE mails ALTER COLUMN service_test SET NOT NULL"
  }
