module Doc.AutomaticReminder.Tables where

import DB
import DB.Checks

createDocumentAutomaticRemindersTable :: MonadDB m => Migration m
createDocumentAutomaticRemindersTable =
  Migration {
      mgrTable = tableDocumentAutomaticReminders
    , mgrFrom = 0
    , mgrDo = do
        createTable $ tblTable {
                        tblName = "document_automatic_reminders"
                      , tblVersion = 1
                      , tblColumns = [
                         tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
                        , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
                        ]
                      , tblPrimaryKey = pkOnColumn "document_id"
                      , tblForeignKeys = [
                         (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
                        ]

                      }
}

tableDocumentAutomaticReminders :: Table
tableDocumentAutomaticReminders = tblTable {
    tblName = "document_automatic_reminders"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "document_id"
  , tblForeignKeys = [
      (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    ]
}
