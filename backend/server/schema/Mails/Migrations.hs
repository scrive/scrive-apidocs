module Mails.Migrations
  ( removeXSMTPAttrsFromMailEvents
  , renameMailAttachmentComposite
  , mailsAddStatsIndexes
  ) where

import DB
import Mails.Tables

renameMailAttachmentComposite :: MonadDB m => Migration m
renameMailAttachmentComposite = Migration
  { mgrTableName = "mail_attachments"
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runSQL_ "ALTER TYPE mail_attachment RENAME TO mail_attachment_c1"
  }

removeXSMTPAttrsFromMailEvents :: MonadDB m => Migration m
removeXSMTPAttrsFromMailEvents = Migration
  { mgrTableName = tblName tableMails
  , mgrFrom      = 6
  , mgrAction    = StandardMigration $ do
                     runSQL_ "ALTER TABLE mails DROP COLUMN x_smtp_attrs"
  }

mailsAddStatsIndexes :: MonadDB m => Migration m
mailsAddStatsIndexes =
  let tname = tblName tableMails
  in  Migration
        { mgrTableName = tname
        , mgrFrom      = 7
        , mgrAction    =
          StandardMigration $ do
            runQuery_ . sqlDropIndex tname $ (indexOnColumns ["reserved_by", "run_at"])
              { idxWhere = Just "reserved_by IS NULL AND run_at IS NOT NULL"
              }
            runQuery_ . sqlCreateIndexSequentially tname $ (indexOnColumn "run_at")
              { idxWhere = Just "run_at IS NOT NULL"
              }
            runQuery_ . sqlCreateIndexSequentially tname $ (indexOnColumn "attempts")
              { idxWhere = Just "attempts > 1 AND finished_at IS NULL"
              }
        }
