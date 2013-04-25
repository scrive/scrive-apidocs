module Mails.Migrations (
    mailerMigrations
  ) where

import DB
import DB.SQL2
import Mails.Tables
import qualified Log as Log

-- Note: ALWAYS append new migrations TO THE END of this list.
mailerMigrations :: MonadDB m => [Migration m]
mailerMigrations = [
    addTestServiceToMails
   , moveAtachmentsToSeparateTable
   , addFileIdToAttachmentsTable
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

moveAtachmentsToSeparateTable :: MonadDB m => Migration m
moveAtachmentsToSeparateTable =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 2
  , mgrDo = do

      kRun_ $ sqlSelect "mails" $ do
        sqlResult "count(*)"
        sqlWhere "attachments IS NOT NULL"
        sqlWhere "attachments <> '[]'"
      let decoder1 :: Int -> Int -> Int
          decoder1 _ value = value
      count <- kFold decoder1 0

      Log.debug $ "There are " ++ show count ++ " mails with attachments to move to mail_attachments, it will take around " ++ show ((count+999) `div` 1000) ++ " minutes"

      kRunRaw $ "WITH"
          <+> "toinsert AS (SELECT mails.id AS id"
          <+> "                  , regexp_matches(attachments, '{\"attName\":\"([^\"]*)\",\"attContent\":\"([^\"]*)\"}', 'g') AS arr"
          <+> "               FROM mails"
          <+> "              WHERE attachments <> '[]')"
          <+> "INSERT INTO mail_attachments(mail_id,name,content)"
          <+> "     SELECT id AS MailID, arr[1] AS Name, decode(arr[2],'base64') AS Content"
          <+> "       FROM toinsert"

      Log.debug "Attachments moved to separate table, now dropping attachments column from mails"

      kRunRaw "ALTER TABLE mails DROP COLUMN attachments"
  }

addFileIdToAttachmentsTable :: MonadDB m => Migration m
addFileIdToAttachmentsTable =
  Migration {
    mgrTable = tableMailAttachments
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw $ "ALTER TABLE mail_attachments ADD COLUMN file_id BIGINT,"
             <> "ALTER COLUMN content DROP NOT NULL"
  }
