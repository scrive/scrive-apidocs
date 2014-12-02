module EID.CGI.GRP.Transaction.Tables where

import Data.Monoid.Space

import DB
import DB.SQLFunction

-- | CGI GRP transaction is unique for a given signatory. Since
-- for a given personal number only one transaction can be active
-- at a time, the constraint is fine. We do not really need to bind
-- transaction to the session, but it gives us garbage collection
-- of old transactions for free.

mergeCgiGrpTransaction :: SQLFunction
mergeCgiGrpTransaction = SQLFunction $ smconcat [
    "CREATE OR REPLACE FUNCTION"
  , "merge_cgi_grp_transaction(signatory_link_id_ BIGINT, session_id_ BIGINT,"
  , " text_to_be_signed_ TEXT, transaction_id_ TEXT, order_ref_ TEXT)"
  , "RETURNS VOID AS $$"
  , "BEGIN"
  , "  LOOP"
  , "    UPDATE cgi_grp_transactions SET"
  , "        session_id = session_id_"
  , "      , text_to_be_signed = text_to_be_signed_"
  , "      , transaction_id = transaction_id_"
  , "      , order_ref = order_ref_"
  , "      WHERE signatory_link_id = signatory_link_id_;"
  , "    IF found THEN"
  , "      RETURN;"
  , "    END IF;"
  , "    BEGIN"
  , "      INSERT INTO cgi_grp_transactions ("
  , "          signatory_link_id"
  , "        , session_id"
  , "        , text_to_be_signed"
  , "        , transaction_id"
  , "        , order_ref"
  , "        ) VALUES ("
  , "          signatory_link_id_"
  , "        , session_id_"
  , "        , text_to_be_signed_"
  , "        , transaction_id_"
  , "        , order_ref_"
  , "        );"
  , "      RETURN;"
  , "    EXCEPTION WHEN unique_violation THEN" -- do nothing, so the loop continues
  , "    END;"
  , "  END LOOP;"
  , "END;"
  , "$$ LANGUAGE plpgsql"
  ]

tableCgiGrpTransactions :: Table
tableCgiGrpTransactions = tblTable {
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
