module Mails.Tables (
    mailerTables
  , tableMails
  , tableMailEvents
  , tableMailAttachments
  ) where

import DB

mailerTables :: [Table]
mailerTables = [
    tableMails
  , tableMailEvents
  , tableMailAttachments
  ]

tableMails :: Table
tableMails = tblTable {
    tblName = "mails"
  , tblVersion = 3
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "sender", colType = TextT, colNullable = False }
    , tblColumn { colName = "receivers", colType = TextT, colNullable = False }
    , tblColumn { colName = "title", colType = TextT }
    , tblColumn { colName = "content", colType = TextT }
    , tblColumn { colName = "x_smtp_attrs", colType = TextT }
    , tblColumn { colName = "to_be_sent", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "sent", colType = TimestampWithZoneT }
    , tblColumn { colName = "service_test", colType = BoolT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  }

tableMailAttachments :: Table
tableMailAttachments = tblTable {
    tblName = "mail_attachments"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "mail_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "content", colType = BinaryT }
    , tblColumn { colName = "file_id", colType = BigIntT }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "mail_id" "mails" "id") { fkOnDelete = ForeignKeyCascade }
    , tblForeignKeyColumn "file_id" "files" "id"
    ]
  , tblIndexes = [tblIndexOnColumn "mail_id"]
  }

tableMailEvents :: Table
tableMailEvents = tblTable {
    tblName = "mail_events"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "mail_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "event", colType = TextT, colNullable = False }
    , tblColumn { colName = "event_read", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "mail_id" "mails" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [tblIndexOnColumn "mail_id"]
  }
