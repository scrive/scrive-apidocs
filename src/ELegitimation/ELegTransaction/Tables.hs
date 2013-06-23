module ELegitimation.ELegTransaction.Tables where

import DB
import DB.SQLFunction

mergeELegTransaction :: SQLFunction
mergeELegTransaction = SQLFunction {
  sqlFunDef  = SQL ("CREATE OR REPLACE FUNCTION"
  <> " merge_eleg_transaction(id_ TEXT, session_id_ BIGINT, nonce_ TEXT,"
  <> "   tbs_ TEXT, encoded_tbs_ TEXT, signatory_link_id_ BIGINT,"
  <> "   document_id_ BIGINT, token_ BIGINT, status_ TEXT, cr_transaction_id_ TEXT,"
  <> "   cr_signature_ TEXT, cr_attributes_ TEXT, oref_ TEXT) RETURNS VOID AS $$"
  <> " BEGIN"
  <> "   LOOP"
  <> "     UPDATE eleg_transactions SET"
  <> "         nonce = nonce_"
  <> "       , tbs = tbs_"
  <> "       , encoded_tbs = encoded_tbs_"
  <> "       , signatory_link_id = signatory_link_id_"
  <> "       , document_id = document_id_"
  <> "       , token = token_"
  <> "       , status = status_"
  <> "       , cr_transaction_id = cr_transaction_id_"
  <> "       , cr_signature = cr_signature_"
  <> "       , cr_attributes = cr_attributes_"
  <> "       , oref = oref_"
  <> "       WHERE id = id_ AND session_id = session_id_;"
  <> "     IF found THEN"
  <> "       RETURN;"
  <> "     END IF;"
  <> "     BEGIN"
  <> "       INSERT INTO eleg_transactions (id, session_id, nonce, tbs, encoded_tbs,"
  <> "         signatory_link_id, document_id, token, status, cr_transaction_id,"
  <> "         cr_signature, cr_attributes, oref)"
  <> "       VALUES (id_, session_id_, nonce_, tbs_, encoded_tbs_, signatory_link_id_,"
  <> "         document_id_, token_, status_, cr_transaction_id_, cr_signature_,"
  <> "         cr_attributes_, oref_);"
  <> "       RETURN;"
  <> "     EXCEPTION WHEN unique_violation THEN" -- do nothing
  <> "     END;"
  <> "   END LOOP;"
  <> " END;"
  <> " $$ LANGUAGE plpgsql") []
}

tableELegTransactions :: Table
tableELegTransactions = tblTable {
    tblName = "eleg_transactions"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = TextT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "nonce", colType = TextT }
    , tblColumn { colName = "tbs", colType = TextT, colNullable = False }
    , tblColumn { colName = "encoded_tbs", colType = TextT }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT }
    , tblColumn { colName = "status", colType = TextT, colNullable = False }
    , tblColumn { colName = "cr_transaction_id", colType = TextT }
    , tblColumn { colName = "cr_signature", colType = TextT }
    , tblColumn { colName = "cr_attributes", colType = TextT }
    , tblColumn { colName = "oref", colType = TextT }
    ]
  , tblPrimaryKey = ["id", "session_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "document_id" "documents" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    , (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  }
