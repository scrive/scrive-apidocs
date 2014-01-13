module SMS.Migrations (
    messengerMigrations
  ) where

import DB
import DB.Checks
import SMS.Tables


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


-- Note: ALWAYS append new migrations TO THE END of this list.
messengerMigrations :: MonadDB m => [Migration m]
messengerMigrations =
  [ createSMSesTable
  , createSMSEventsTable
  ]
