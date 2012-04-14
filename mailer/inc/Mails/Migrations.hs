module Mails.Migrations (
    mailerMigrations
  ) where

import DB
import Mails.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
mailerMigrations :: [Migration]
mailerMigrations = [
    addTestServiceToMails
  ]

addTestServiceToMails :: Migration
addTestServiceToMails =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE mails ADD COLUMN service_test BOOL"
      _ <- kRun $ SQL "UPDATE mails SET service_test = ?" [toSql False]
      kRunRaw "ALTER TABLE mails ALTER COLUMN service_test SET NOT NULL"
  }
