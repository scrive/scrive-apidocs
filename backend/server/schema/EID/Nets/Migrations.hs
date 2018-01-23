module EID.Nets.Migrations where

import Database.PostgreSQL.PQTypes.Checks

import DB
import EID.Nets.Tables

createNetsSignOrdersTable :: MonadDB m => Migration m
createNetsSignOrdersTable = Migration {
  mgrTableName = tblName tableNetsSignOrders
, mgrFrom = 0
, mgrAction = StandardMigration $ createTable True tblTable {
    tblName = "nets_sign_orders"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "text_to_be_signed", colType = TextT, colNullable = False }
    , tblColumn { colName = "order_id", colType = TextT, colNullable = False }
    , tblColumn { colName = "ssn", colType = TextT, colNullable = False }
    , tblColumn { colName = "deadline", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "is_canceled", colType = BoolT, colNullable = False }
    ]
  -- only one authentication per signatory. can be relaxed later if necessary.
  , tblPrimaryKey = pkOnColumn "signatory_link_id"
  , tblForeignKeys = [
      (fkOnColumn "session_id" "sessions" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [
      indexOnColumn "session_id"
    , (indexOnColumn "order_id") { idxUnique = True }
    ]
  }
}
