module EID.CGI.GRP.Transaction.Tables where

import DB

-- | CGI GRP transaction is unique for a given signatory. Since
-- for a given personal number only one transaction can be active
-- at a time, the constraint is fine. We do not really need to bind
-- transaction to the session, but it gives us garbage collection
-- of old transactions for free.

tableCgiGrpTransactions :: Table
tableCgiGrpTransactions = tblTable
  { tblName        = "cgi_grp_transactions"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "text_to_be_signed", colType = TextT }
    , tblColumn { colName = "transaction_id", colType = TextT, colNullable = False }
    , tblColumn { colName = "order_ref", colType = TextT, colNullable = False }
    , tblColumn { colName = "type", colType = SmallIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumns ["signatory_link_id", "type"]
  , tblForeignKeys =
    [ (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [indexOnColumn "signatory_link_id", indexOnColumn "session_id"]
  }
