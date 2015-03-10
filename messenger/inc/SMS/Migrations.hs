module SMS.Migrations (
    messengerMigrations
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid

import DB
import DB.Checks
import SMS.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
messengerMigrations :: MonadDB m => [Migration m]
messengerMigrations =
  [ createSMSesTable
  , createSMSEventsTable
  , addAttemptCountToSMSes
  , createTableMessengerWorkers
  , makeSMSesTableJobQueue
  , createTableMessengerJobs
  ]

----------------------------------------

createTableMessengerJobs :: MonadDB m => Migration m
createTableMessengerJobs = Migration {
  mgrTable = tableMessengerJobs
, mgrFrom = 0
, mgrDo = do
  createTable tblTable {
    tblName = "messenger_jobs"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = TextT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False, colDefault = Just "0" }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "reserved_by" "messenger_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  }
  forM_ jobs $ \job -> do
    runSQL_ $ "INSERT INTO messenger_jobs (id, run_at) VALUES (" <?> job <> ", to_timestamp(0))"
}
  where
    jobs :: [ByteString]
    jobs = [
        "clean_old_smses"
      ]

createTableMessengerWorkers :: MonadDB m => Migration m
createTableMessengerWorkers = Migration {
  mgrTable = tableMessengerWorkers
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "messenger_workers"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }
}

makeSMSesTableJobQueue :: MonadDB m => Migration m
makeSMSesTableJobQueue = Migration {
  mgrTable = tableSMSes
, mgrFrom = 2
, mgrDo = do
  let tname = tblName tableSMSes
      alterTable = sqlAlterTable tname
  runQuery_ $ alterTable ["RENAME COLUMN to_be_sent TO run_at"]
  runQuery_ $ alterTable ["RENAME COLUMN sent TO finished_at"]
  runQuery_ $ alterTable ["RENAME COLUMN attempt TO attempts"]
  runQuery_ $ alterTable [
      "ALTER COLUMN run_at DROP NOT NULL"
    , sqlAddColumn tblColumn { colName = "reserved_by", colType = BigIntT }
    , sqlAddFK tname (fkOnColumn "reserved_by" "messenger_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  runQuery_ $ sqlCreateIndex tname (indexOnColumns ["reserved_by", "run_at"]) {
    idxWhere = Just "reserved_by IS NULL AND run_at IS NOT NULL"
  }
}

createSMSesTable  :: MonadDB m => Migration m
createSMSesTable =
  Migration {
      mgrTable = tableSMSes
    , mgrFrom = 0
    , mgrDo = do
       createTable $ tblTable {
                       tblName = "smses"
                     , tblVersion = 1
                     , tblColumns = [
                        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                       , tblColumn { colName = "originator", colType = TextT, colNullable = False }
                       , tblColumn { colName = "msisdn", colType = TextT, colNullable = False }
                       , tblColumn { colName = "body", colType = TextT, colNullable = False }
                       , tblColumn { colName = "to_be_sent", colType = TimestampWithZoneT, colNullable = False }
                       , tblColumn { colName = "sent", colType = TimestampWithZoneT }
                       , tblColumn { colName = "data", colType = TextT, colNullable = False }
                       ]
                     , tblPrimaryKey = pkOnColumn "id"
                     }
       return ()
    }

createSMSEventsTable  :: MonadDB m => Migration m
createSMSEventsTable =
  Migration {
      mgrTable = tableSMSEvents
    , mgrFrom = 0
    , mgrDo = do
       createTable $ tblTable {
                       tblName = "sms_events"
                     , tblVersion = 1
                     , tblColumns = [
                        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                       , tblColumn { colName = "sms_id", colType = BigIntT, colNullable = False }
                       , tblColumn { colName = "event", colType = TextT, colNullable = False }
                       , tblColumn { colName = "event_read", colType = TimestampWithZoneT }
                       ]
                     , tblPrimaryKey = pkOnColumn "id"
                     , tblForeignKeys = [
                        (fkOnColumn "sms_id" "smses" "id") { fkOnDelete = ForeignKeyCascade }
                       ]
                     , tblIndexes = [indexOnColumn "sms_id"]
                     }
       return ()
  }


addAttemptCountToSMSes :: MonadDB m => Migration m
addAttemptCountToSMSes =
  Migration {
    mgrTable = tableSMSes
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE smses ADD COLUMN attempt INTEGER NOT NULL DEFAULT 0"
  }
