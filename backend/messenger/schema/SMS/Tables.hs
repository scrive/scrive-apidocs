module SMS.Tables (
    messengerTables
  , tableMessengerJobs
  , tableMessengerWorkers
  , tableSMSes
  , tableSMSEvents
  ) where

import Data.Text (Text)

import DB

messengerTables :: [Table]
messengerTables = [
    tableMessengerWorkers
  , tableMessengerJobs
  , tableSMSes
  , tableSMSEvents
  ]

----------------------------------------

tableMessengerWorkers :: Table
tableMessengerWorkers = tblTable {
  tblName = "messenger_workers"
, tblVersion = 1
, tblColumns = [
    tblColumn { colName = "id",            colType = BigSerialT
              , colNullable = False }
  , tblColumn { colName = "last_activity", colType = TimestampWithZoneT
              , colNullable = False }
  , tblColumn { colName = "name",          colType = TextT
              , colNullable = False }
  ]
, tblPrimaryKey = pkOnColumn "id"
}

tableMessengerJobs :: Table
tableMessengerJobs = tblTable {
  tblName = "messenger_jobs"
, tblVersion = 1
, tblColumns = [
    tblColumn { colName = "id",          colType = TextT
              , colNullable = False }
  , tblColumn { colName = "run_at",      colType = TimestampWithZoneT
              , colNullable = False }
  , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
  , tblColumn { colName = "reserved_by", colType = BigIntT }
  , tblColumn { colName = "attempts",    colType = IntegerT
              , colNullable = False,     colDefault = Just "0" }
  ]
, tblPrimaryKey = pkOnColumn "id"
, tblForeignKeys = [
    (fkOnColumn "reserved_by" "messenger_workers" "id") {
      fkOnDelete = ForeignKeySetNull
    }
  ]
, tblInitialSetup = Just $ TableInitialSetup {
    checkInitialSetup = return True
  , initialSetup = forM_ jobs $ \job -> do
    runSQL_ $ "INSERT INTO messenger_jobs (id, run_at) VALUES ("
      <?> job <> ", to_timestamp(0))"
  }
}
  where
    jobs :: [Text]
    jobs = [
        "clean_old_smses"
      ]

tableSMSes :: Table
tableSMSes = tblTable {
    tblName = "smses"
  , tblVersion = 6
  , tblColumns = [
      tblColumn { colName = "id",          colType = BigSerialT
                , colNullable = False }
    , tblColumn { colName = "originator",  colType = TextT, colNullable = False }
    , tblColumn { colName = "msisdn",      colType = TextT, colNullable = False }
    , tblColumn { colName = "body",        colType = TextT, colNullable = False }
    , tblColumn { colName = "run_at",      colType = TimestampWithZoneT }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "attempts",    colType = IntegerT
                , colNullable = False,     colDefault = Just "0"}
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "provider",    colType = SmallIntT
                , colNullable = False,     colDefault = Just "1"}
    , tblColumn { colName = "telia_id",    colType = TextT }
    , tblColumn { colName = "mblox_id",    colType = TextT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "reserved_by" "messenger_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  , tblIndexes = [
      (indexOnColumns ["reserved_by", "run_at"]) {
        idxWhere = Just "reserved_by IS NULL AND run_at IS NOT NULL"
      }
    ]
  }

tableSMSEvents :: Table
tableSMSEvents = tblTable {
    tblName = "sms_events"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id",         colType = BigSerialT
                , colNullable = False }
    , tblColumn { colName = "sms_id",     colType = BigIntT, colNullable = False }
    , tblColumn { colName = "event",      colType = TextT,   colNullable = False }
    , tblColumn { colName = "event_read", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "sms_id" "smses" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [indexOnColumn "sms_id"]
  }
