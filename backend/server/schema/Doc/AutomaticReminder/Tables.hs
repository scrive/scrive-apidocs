module Doc.AutomaticReminder.Tables where

import DB

tableDocumentAutomaticReminders :: Table
tableDocumentAutomaticReminders = tblTable
  { tblName        = "document_automatic_reminders"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "document_id"
  , tblForeignKeys =
    [(fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }]
  }
