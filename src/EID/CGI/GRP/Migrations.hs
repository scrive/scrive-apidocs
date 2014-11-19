module EID.CGI.GRP.Migrations where

import DB
import DB.Checks
import EID.CGI.GRP.Tables

createCgiGrpTransactionsTable :: MonadDB m => Migration m
createCgiGrpTransactionsTable = Migration {
  mgrTable = tableCgiGrpTransactions
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "cgi_grp_transactions"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "session_id",        colType = BigIntT, colNullable = False }
    , tblColumn { colName = "transaction_id",    colType = TextT,   colNullable = False }
    , tblColumn { colName = "order_ref",         colType = TextT,   colNullable = False }
    ]
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
      indexOnColumn "signatory_link_id"
    , indexOnColumn "session_id"
    ]
  }
}
