module EID.CGI.GRP.Transaction.Migrations where

import DB
import DB.Checks
import EID.CGI.GRP.Transaction.Tables
import KontraPrelude

createCgiGrpTransactionsTable :: MonadDB m => Migration m
createCgiGrpTransactionsTable = Migration {
  mgrTable = tableCgiGrpTransactions
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "cgi_grp_transactions"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "text_to_be_signed", colType = TextT, colNullable = False }
    , tblColumn { colName = "transaction_id", colType = TextT, colNullable = False }
    , tblColumn { colName = "order_ref", colType = TextT, colNullable = False }
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

extendCgiGrpTransactionsWithAuthRequest :: MonadDB m => Migration m
extendCgiGrpTransactionsWithAuthRequest =
  Migration
    { mgrTable = tableCgiGrpTransactions
    , mgrFrom = 1
    , mgrDo = do
        runSQL_ "ALTER TABLE cgi_grp_transactions ADD COLUMN type SMALLINT NOT NULL DEFAULT 2" -- 2 is CgiGrpSign
        runSQL_ "ALTER TABLE cgi_grp_transactions ALTER COLUMN type DROP DEFAULT"
        runSQL_ "ALTER TABLE cgi_grp_transactions ALTER COLUMN text_to_be_signed DROP NOT NULL"
        runQuery_ $ "ALTER TABLE cgi_grp_transactions " <> sqlDropPK (tblName tableCgiGrpTransactions)
        runQuery_ $ "ALTER TABLE cgi_grp_transactions " <> sqlAddPK (tblName tableCgiGrpTransactions) ($fromJust $ pkOnColumns ["signatory_link_id","type"])
    }
