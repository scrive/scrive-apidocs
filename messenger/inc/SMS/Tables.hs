module SMS.Tables (
    messengerTables
  , tableSMSes
  , tableSMSEvents
  ) where

import DB

messengerTables :: [Table]
messengerTables = [
    tableSMSes
  , tableSMSEvents
  ]

tableSMSes :: Table
tableSMSes = tblTable {
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
  , tblPrimaryKey = ["id"]
  }

tableSMSEvents :: Table
tableSMSEvents = tblTable {
    tblName = "sms_events"
  , tblVersion = 1
  , tblColumns = [
    tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "sms_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "event", colType = TextT, colNullable = False }
    , tblColumn { colName = "event_read", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "sms_id" "smses" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [tblIndexOnColumn "sms_id"]
  }
