module Mails.Migrations (
    mailerMigrations
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Int

import DB
import DB.Checks
import KontraPrelude
import Log
import Mails.Tables
import MinutesTime

-- Note: ALWAYS append new migrations TO THE END of this list.
mailerMigrations :: (MonadDB m, MonadThrow m, MonadLog m) => [Migration m]
mailerMigrations = [
    addTestServiceToMails
  , moveAtachmentsToSeparateTable
  , addFileIdToAttachmentsTable
  , addAttemptCountToMails
  , addReplyToToMails
  , createMailerWorkersTable
  , makeMailsTableJobQueue
  , createMailerJobsTable
  ]

----------------------------------------

createMailerJobsTable :: MonadDB m => Migration m
createMailerJobsTable = Migration {
  mgrTable = tableMailerJobs
, mgrFrom = 0
, mgrDo = do
  createTable tblTable {
    tblName = "mailer_jobs"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = TextT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False, colDefault = Just "0" }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "reserved_by" "mailer_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  }
  forM_ jobs $ \values -> runQuery_ $ rawSQL
    "INSERT INTO mailer_jobs (id, run_at, finished_at) VALUES ($1, $2, $3)"
    values
}
  where
    jobs :: [(ByteString, Maybe UTCTime, Maybe UTCTime)]
    jobs = [
        ("clean_old_emails", Just unixEpoch, Nothing)
      , ("perform_service_test", Just unixEpoch, Nothing)
      , ("collect_service_test_result", Nothing, Just unixEpoch)
      ]

createMailerWorkersTable :: MonadDB m => Migration m
createMailerWorkersTable = Migration {
  mgrTable = tableMailerWorkers
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "mailer_workers"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }
}

makeMailsTableJobQueue :: MonadDB m => Migration m
makeMailsTableJobQueue = Migration {
  mgrTable = tableMails
, mgrFrom = 5
, mgrDo = do
  let tname = tblName tableMails
      alterTable = sqlAlterTable tname
  runQuery_ $ alterTable ["RENAME COLUMN to_be_sent TO run_at"]
  runQuery_ $ alterTable ["RENAME COLUMN sent TO finished_at"]
  runQuery_ $ alterTable ["RENAME COLUMN attempt TO attempts"]
  runQuery_ $ alterTable [
      "ALTER COLUMN title SET NOT NULL"
    , "ALTER COLUMN content SET NOT NULL"
    , "ALTER COLUMN x_smtp_attrs SET NOT NULL"
    , "ALTER COLUMN run_at DROP NOT NULL"
    , "ADD COLUMN reserved_by BIGINT"
    , sqlAddFK tname (fkOnColumn "reserved_by" "mailer_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  runQuery_ $ sqlCreateIndex tname (indexOnColumns ["reserved_by", "run_at"]) {
    idxWhere = Just "reserved_by IS NULL AND run_at IS NOT NULL"
  }
  runQuery_ $ sqlCreateIndex tname (indexOnColumn "service_test") {
    idxWhere = Just "service_test = true"
  }
}

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

moveAtachmentsToSeparateTable :: (MonadDB m, MonadThrow m, MonadLog m) => Migration m
moveAtachmentsToSeparateTable =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 2
  , mgrDo = do

      runQuery_ . sqlSelect "mails" $ do
        sqlResult "count(*)"
        sqlWhere "attachments IS NOT NULL"
        sqlWhere "attachments <> '[]'"
      count :: Int64 <- fetchOne runIdentity

      logInfo_ $ "There are " ++ show count ++ " mails with attachments to move to mail_attachments, it will take around " ++ show ((count+999) `div` 1000) ++ " minutes"

      runSQL_ $ "WITH"
          <+> "toinsert AS (SELECT mails.id AS id"
          <+> "                  , regexp_matches(attachments, '{\"attName\":\"([^\"]*)\",\"attContent\":\"([^\"]*)\"}', 'g') AS arr"
          <+> "               FROM mails"
          <+> "              WHERE attachments <> '[]')"
          <+> "INSERT INTO mail_attachments(mail_id,name,content)"
          <+> "     SELECT id AS MailID, arr[1] AS Name, decode(arr[2],'base64') AS Content"
          <+> "       FROM toinsert"

      logInfo_ "Attachments moved to separate table, now dropping attachments column from mails"

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
