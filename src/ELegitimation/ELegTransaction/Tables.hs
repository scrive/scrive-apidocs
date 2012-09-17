module ELegitimation.ELegTransaction.Tables where

import DB
import DB.SQLFunction

mergeELegTransaction :: SQLFunction
mergeELegTransaction = SQLFunction {
    sqlFunHeader = "merge_eleg_transaction(TEXT, BIGINT, TEXT, TEXT, TEXT, BIGINT, BIGINT, BIGINT, TEXT, TEXT, TEXT, TEXT, TEXT)"
  , sqlFunDef  = SQL ("CREATE FUNCTION"
    ++ " merge_eleg_transaction(id_ TEXT, session_id_ BIGINT, nonce_ TEXT,"
    ++ "   tbs_ TEXT, encoded_tbs_ TEXT, signatory_link_id_ BIGINT,"
    ++ "   document_id_ BIGINT, token_ BIGINT, status_ TEXT, cr_transaction_id_ TEXT,"
    ++ "   cr_signature_ TEXT, cr_attributes_ TEXT, oref_ TEXT) RETURNS VOID AS $$"
    ++ " BEGIN"
    ++ "   LOOP"
    ++ "     UPDATE eleg_transactions SET"
    ++ "         nonce = nonce_"
    ++ "       , tbs = tbs_"
    ++ "       , encoded_tbs = encoded_tbs_"
    ++ "       , signatory_link_id = signatory_link_id_"
    ++ "       , document_id = document_id_"
    ++ "       , token = token_"
    ++ "       , status = status_"
    ++ "       , cr_transaction_id = cr_transaction_id_"
    ++ "       , cr_signature = cr_signature_"
    ++ "       , cr_attributes = cr_attributes_"
    ++ "       , oref = oref_"
    ++ "       WHERE id = id_ AND session_id = session_id_;"
    ++ "     IF found THEN"
    ++ "       RETURN;"
    ++ "     END IF;"
    ++ "     BEGIN"
    ++ "       INSERT INTO eleg_transactions (id, session_id, nonce, tbs, encoded_tbs,"
    ++ "         signatory_link_id, document_id, token, status, cr_transaction_id,"
    ++ "         cr_signature, cr_attributes, oref)"
    ++ "       VALUES (id_, session_id_, nonce_, tbs_, encoded_tbs_, signatory_link_id_,"
    ++ "         document_id_, token_, status_, cr_transaction_id_, cr_signature_,"
    ++ "         cr_attributes_, oref_);"
    ++ "       RETURN;"
    ++ "     EXCEPTION WHEN unique_violation THEN" -- do nothing
    ++ "     END;"
    ++ "   END LOOP;"
    ++ " END;"
    ++ " $$ LANGUAGE plpgsql") []
}

tableELegTransactions :: Table
tableELegTransactions = Table {
    tblName = "eleg_transactions"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("session_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("nonce", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("tbs", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("encoded_tbs", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("status", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("cr_transaction_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("cr_signature", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("cr_attributes", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("oref", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE eleg_transactions ("
          ++ "  id TEXT NOT NULL"
          ++ ", session_id BIGINT NOT NULL"
          ++ ", nonce TEXT NULL"
          ++ ", tbs TEXT NOT NULL"
          ++ ", encoded_tbs TEXT NULL"
          ++ ", signatory_link_id BIGINT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", token BIGINT NULL"
          ++ ", status TEXT NOT NULL"
          ++ ", cr_transaction_id TEXT NULL"
          ++ ", cr_signature TEXT NULL"
          ++ ", cr_attributes TEXT NULL"
          ++ ", oref TEXT NULL"
          ++ ", CONSTRAINT pk_eleg_transactions PRIMARY KEY (id, session_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE eleg_transactions"
      ++ " ADD CONSTRAINT fk_eleg_transactions_session_id FOREIGN KEY(session_id)"
      ++ " REFERENCES sessions(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE eleg_transactions"
      ++ " ADD CONSTRAINT fk_eleg_transactions_signatory_link_id FOREIGN KEY(signatory_link_id)"
      ++ " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE eleg_transactions"
      ++ " ADD CONSTRAINT fk_eleg_transactions_document_id FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
}
