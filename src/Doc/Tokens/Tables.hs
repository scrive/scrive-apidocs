module Doc.Tokens.Tables where

import Data.Monoid ((<>))
import DB
import DB.SQLFunction

insertDocumentSessionToken :: SQLFunction
insertDocumentSessionToken = SQLFunction {
  sqlFunDef = SQL ("CREATE OR REPLACE FUNCTION"
  <> " insert_document_session_token(session_id_ BIGINT, signatory_link_id_ BIGINT,"
  <> "                               token_ BIGINT) RETURNS BOOLEAN AS $$"
  <> " BEGIN"
  <> "   LOOP"
  <> "     UPDATE document_session_tokens SET token = token_"
  <> "       WHERE session_id = session_id_"
  <> "         AND signatory_link_id = signatory_link_id_;"
  <> "     IF found THEN"
  <> "       RETURN TRUE;"
  <> "     END IF;"
  <> "     BEGIN"
  <> "       INSERT INTO document_session_tokens(session_id, signatory_link_id, token)"
  <> "         VALUES (session_id_, signatory_link_id_, token_);"
  <> "       RETURN TRUE;"
  <> "     EXCEPTION"
  <> "       WHEN unique_violation THEN" -- do nothing, let update be run again
  <> "       WHEN foreign_key_violation THEN" -- invalid values, return false
  <> "         RETURN FALSE;"
  <> "     END;"
  <> "   END LOOP;"
  <> " END;"
  <> " $$ LANGUAGE plpgsql") []
}

tableDocumentSessionTokens :: Table
tableDocumentSessionTokens = tblTable {
    tblName = "document_session_tokens"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("session_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE document_session_tokens ("
          <> "  session_id BIGINT NOT NULL"
          <> ", signatory_link_id BIGINT NULL"
          <> ", token BIGINT NOT NULL"
          <> ", CONSTRAINT pk_document_session_tokens PRIMARY KEY (session_id, signatory_link_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE document_session_tokens"
      <> " ADD CONSTRAINT fk_document_session_tokens_session_id FOREIGN KEY(session_id)"
      <> " REFERENCES sessions(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE document_session_tokens"
      <> " ADD CONSTRAINT fk_document_session_tokens_signatory_link_id FOREIGN KEY(signatory_link_id)"
      <> " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
}
