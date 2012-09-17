module Doc.Tickets.Tables where

import DB
import DB.SQLFunction

insertDocumentTicket :: SQLFunction
insertDocumentTicket = SQLFunction {
  sqlFunDef = SQL ("CREATE OR REPLACE FUNCTION"
  ++ " insert_document_ticket(session_id_ BIGINT, signatory_link_id_ BIGINT,"
  ++ "                        token_ BIGINT) RETURNS BOOLEAN AS $$"
  ++ " BEGIN"
  ++ "   INSERT INTO document_tickets(session_id, signatory_link_id, token)"
  ++ "     VALUES (session_id_, signatory_link_id_, token_);"
  ++ "   RETURN TRUE;"
  ++ " EXCEPTION"
  ++ "   WHEN unique_violation THEN" -- ticket is already there, update token
  ++ "     UPDATE document_tickets SET token = token_"
  ++ "       WHERE session_id = session_id_"
  ++ "         AND signatory_link_id = signatory_link_id_;"
  ++ "     RETURN found;"
  ++ "   WHEN foreign_key_violation THEN" -- invalid values
  ++ "     RETURN FALSE;"
  ++ " END;"
  ++ " $$ LANGUAGE plpgsql") []
}

tableDocumentTickets :: Table
tableDocumentTickets = Table {
    tblName = "document_tickets"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("session_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE document_tickets ("
          ++ "  session_id BIGINT NOT NULL"
          ++ ", signatory_link_id BIGINT NULL"
          ++ ", token BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_document_tickets PRIMARY KEY (session_id, signatory_link_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE document_tickets"
      ++ " ADD CONSTRAINT fk_document_tickets_session_id FOREIGN KEY(session_id)"
      ++ " REFERENCES sessions(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE document_tickets"
      ++ " ADD CONSTRAINT fk_document_tickets_signatory_link_id FOREIGN KEY(signatory_link_id)"
      ++ " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
}
