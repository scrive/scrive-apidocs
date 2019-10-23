module Mails.Migrations
  ( removeXSMTPAttrsFromMailEvents
  , renameMailAttachmentComposite
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
