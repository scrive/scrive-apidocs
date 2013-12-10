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
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "originator", colType = TextT, colNullable = False }
    , tblColumn { colName = "msisdn", colType = TextT, colNullable = False }
    , tblColumn { colName = "body", colType = TextT, colNullable = False }
    , tblColumn { colName = "to_be_sent", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "sent", colType = TimestampWithZoneT }
    , tblColumn { colName = "data", colType = TextT, colNullable = False }
    , tblColumn { colName = "attempt", colType = IntegerT, colNullable = False, colDefault = Just "0"}

    ]
  , tblPrimaryKey = pkOnColumn "id"
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
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "sms_id" "smses" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [indexOnColumn "sms_id"]
  }
