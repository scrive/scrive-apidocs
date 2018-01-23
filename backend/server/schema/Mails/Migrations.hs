module Mails.Migrations (removeXSMTPAttrsFromMailEvents) where

import DB
import Mails.Tables

removeXSMTPAttrsFromMailEvents :: MonadDB m => Migration m
removeXSMTPAttrsFromMailEvents = Migration {
    mgrTableName = tblName tableMails
  , mgrFrom = 6
  , mgrAction = StandardMigration $ do
      runSQL_ "ALTER TABLE mails DROP COLUMN x_smtp_attrs"
  }
