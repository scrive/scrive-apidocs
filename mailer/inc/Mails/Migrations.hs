module Mails.Migrations (
    mailerMigrations
  ) where

import Data.Int
import Data.Monoid.Space

import DB
import Mails.Tables
import qualified Log

-- Note: ALWAYS append new migrations TO THE END of this list.
mailerMigrations :: (MonadDB m, Log.MonadLog m) => [Migration m]
mailerMigrations = [
    addTestServiceToMails
  , moveAtachmentsToSeparateTable
  , addFileIdToAttachmentsTable
  , addAttemptCountToMails
  , addReplyToToMails
  ]

addTestServiceToMails :: MonadDB m => Migration m
addTestServiceToMails =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE mails ADD COLUMN service_test BOOL"
      runSQL_ $ "UPDATE mails SET service_test =" <?> False
      runSQL_ "ALTER TABLE mails ALTER COLUMN service_test SET NOT NULL"
  }

moveAtachmentsToSeparateTable :: (MonadDB m, Log.MonadLog m) => Migration m
moveAtachmentsToSeparateTable =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 2
  , mgrDo = do

      runQuery_ . sqlSelect "mails" $ do
        sqlResult "count(*)"
        sqlWhere "attachments IS NOT NULL"
        sqlWhere "attachments <> '[]'"
      count :: Int64 <- fetchOne unSingle

      Log.mixlog_ $ "There are " ++ show count ++ " mails with attachments to move to mail_attachments, it will take around " ++ show ((count+999) `div` 1000) ++ " minutes"

      runSQL_ $ "WITH"
          <+> "toinsert AS (SELECT mails.id AS id"
          <+> "                  , regexp_matches(attachments, '{\"attName\":\"([^\"]*)\",\"attContent\":\"([^\"]*)\"}', 'g') AS arr"
          <+> "               FROM mails"
          <+> "              WHERE attachments <> '[]')"
          <+> "INSERT INTO mail_attachments(mail_id,name,content)"
          <+> "     SELECT id AS MailID, arr[1] AS Name, decode(arr[2],'base64') AS Content"
          <+> "       FROM toinsert"

      Log.mixlog_ "Attachments moved to separate table, now dropping attachments column from mails"

      runSQL_ "ALTER TABLE mails DROP COLUMN attachments"
  }

addAttemptCountToMails :: MonadDB m => Migration m
addAttemptCountToMails =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 3
  , mgrDo = do
      runSQL_ "ALTER TABLE mails ADD COLUMN attempt INTEGER NOT NULL DEFAULT 0"
  }

addFileIdToAttachmentsTable :: MonadDB m => Migration m
addFileIdToAttachmentsTable =
  Migration {
    mgrTable = tableMailAttachments
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ $ "ALTER TABLE mail_attachments ADD COLUMN file_id BIGINT,"
            <+> "ALTER COLUMN content DROP NOT NULL"
  }

addReplyToToMails :: MonadDB m => Migration m
addReplyToToMails =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 4
  , mgrDo = do
      runSQL_ "ALTER TABLE mails ADD COLUMN reply_to TEXT"
  }
