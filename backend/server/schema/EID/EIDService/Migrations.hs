module EID.EIDService.Migrations (
    createEIDServiceTransactionsTable
  ) where

import Database.PostgreSQL.PQTypes.Checks

import DB

createEIDServiceTransactionsTable :: MonadDB m => Migration m
createEIDServiceTransactionsTable = Migration {
  mgrTableName = "eid_service_transactions"
, mgrFrom = 0
, mgrAction = StandardMigration $ createTable True tblTable {
    tblName = "eid_service_transactions"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "transaction_id", colType = TextT, colNullable = False }
    , tblColumn { colName = "status", colType = SmallIntT, colNullable = False}
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "auth_kind", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False}
    , tblColumn { colName = "deadline", colType = TimestampWithZoneT, colNullable = False }
    ]
  -- only one authentication per signatory. can be relaxed later if necessary.
  , tblPrimaryKey = pkOnColumns ["signatory_link_id", "auth_kind"]
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
    , (indexOnColumn "transaction_id") { idxUnique = True }
    ]
  }
}
