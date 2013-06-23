module Doc.Tokens.Tables where

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
  , tblColumns = [
      tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["session_id", "signatory_link_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  }
