module SMS.Migrations (
    messengerMigrations
  ) where

import DB
import SMS.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
messengerMigrations :: MonadDB m => [Migration m]
messengerMigrations = [
  addSignatoryLinkIDToSmses
  ]

addSignatoryLinkIDToSmses :: MonadDB m => Migration m
addSignatoryLinkIDToSmses =
  Migration {
    mgrTable = tableSMSes
  , mgrFrom = 1
  , mgrDo = do
       kRun_ ("ALTER TABLE smses ADD COLUMN signatory_link_id    BIGINT" :: SQL)
  }
