module Doc.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableDocuments :: Table
tableDocuments = Table {
    tblName = "documents"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("service_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("title", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("status", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("type", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("process", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("functionality", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("ctime", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("mtime", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("days_to_sign", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("timeout_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("invite_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("invite_ip", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("log", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("invite_text", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("trust_weaver_reference", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("allowed_id_types", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("csv_title", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_contents", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("csv_signatory_index", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("cancelation_reason", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("sharing", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("rejection_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("rejection_signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("rejection_reason", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("tags", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("mail_footer", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("region", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE documents ("
          ++ "  id BIGINT NOT NULL"
          ++ ", service_id TEXT NOT NULL"
          ++ ", title TEXT NOT NULL"
          ++ ", status TEXT NOT NULL"
          ++ ", type SMALLINT NOT NULL"
          ++ ", process SMALLINT NOT NULL"
          ++ ", functionality SMALLINT NOT NULL"
          ++ ", ctime TIMESTAMPTZ NOT NULL"
          ++ ", mtime TIMESTAMPTZ NOT NULL"
          ++ ", days_to_sign INTEGER NULL"
          ++ ", timeout_time TIMESTAMPTZ NULL"
          ++ ", invite_time TIMESTAMPTZ NULL"
          ++ ", invite_ip INTEGER NULL"
          ++ ", log TEXT NOT NULL"
          ++ ", invite_text TEXT NOT NULL"
          ++ ", trust_weaver_reference TEXT NULL"
          ++ ", allowed_id_types INTEGER NOT NULL"
          ++ ", csv_title TEXT NULL"
          ++ ", csv_contents BYTEA NULL"
          ++ ", csv_signatory_index INTEGER NULL"
          ++ ", cancelation_reason TEXT NULL"
          ++ ", sharing SMALLINT NOT NULL"
          ++ ", rejection_time TIMESTAMPTZ NULL"
          ++ ", rejection_signatory_link_id BIGINT NULL"
          ++ ", rejection_reason TEXT NULL"
          ++ ", tags TEXT NOT NULL"
          ++ ", mail_footer TEXT NULL"
          ++ ", region SMALLINT NOT NULL"
          ++ ", deleted BOOL NOT NULL"
          ++ ", CONSTRAINT pk_documents PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "CREATE INDEX idx_documents_service_id ON documents(service_id)"
    runRaw conn $ "ALTER TABLE documents"
      ++ " ADD CONSTRAINT fk_documents_services FOREIGN KEY(service_id)"
      ++ " REFERENCES services(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableDocumentFiles :: Table
tableDocumentFiles = Table {
    tblName = "document_files"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("type", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("storage", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE document_files ("
          ++ "  id BIGINT NOT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", type TEXT NOT NULL"
          ++ ", name TEXT NOT NULL"
          ++ ", storage TEXT NULL"
          ++ ", CONSTRAINT pk_document_files PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "CREATE INDEX idx_document_files_document_id ON document_files(document_id)"
    runRaw conn $ "ALTER TABLE document_files"
      ++ " ADD CONSTRAINT fk_document_files_documents FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableSignatoryLinks :: Table
tableSignatoryLinks = Table {
    tblName = "signatory_links"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("company_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("fields", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("sign_order", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sign_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("sign_ip", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("seen_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("seen_ip", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("read_invitation", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("invitation_delivery_status", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("signinfo_text", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signinfo_signature", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signinfo_certificate", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signinfo_provider", SqlColDesc {colType = SqlSmallIntT, colNullable = Just True})
       , ("signinfo_first_name_verified", SqlColDesc {colType = SqlBitT, colNullable = Just True})
       , ("signinfo_last_name_verified", SqlColDesc {colType = SqlBitT, colNullable = Just True})
       , ("signinfo_personal_number_verified", SqlColDesc {colType = SqlBitT, colNullable = Just True})
       , ("roles", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("really_deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE signatory_links ("
          ++ "  id BIGINT NOT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", user_id BIGINT NULL"
          ++ ", company_id BIGINT NULL"
          ++ ", fields TEXT NOT NULL"
          ++ ", sign_order INTEGER NOT NULL"
          ++ ", token BIGINT NOT NULL"
          ++ ", sign_time TIMESTAMPTZ NULL"
          ++ ", sign_ip INTEGER NULL"
          ++ ", seen_time TIMESTAMPTZ NULL"
          ++ ", seen_ip INTEGER NULL"
          ++ ", read_invitation TIMESTAMPTZ NULL"
          ++ ", invitation_delivery_status SMALLINT NOT NULL"
          ++ ", signinfo_text TEXT NULL"
          ++ ", signinfo_signature TEXT NULL"
          ++ ", signinfo_certificate TEXT NULL"
          ++ ", signinfo_provider SMALLINT NULL"
          ++ ", signinfo_first_name_verified BOOL NULL"
          ++ ", signinfo_last_name_verified BOOL NULL"
          ++ ", signinfo_personal_number_verified BOOL NULL"
          ++ ", roles INTEGER NOT NULL"
          ++ ", deleted BOOL NOT NULL"
          ++ ", really_deleted BOOL NOT NULL"
          ++ ", CONSTRAINT pk_signatory_links PRIMARY KEY (id, document_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "CREATE INDEX idx_signatory_links_user_id ON signatory_links(user_id)"
    runRaw conn $ "CREATE INDEX idx_signatory_links_company_id ON signatory_links(company_id)"
    runRaw conn $ "ALTER TABLE signatory_links"
      ++ " ADD CONSTRAINT fk_signatory_links_document_id FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE signatory_links"
      ++ " ADD CONSTRAINT fk_signatory_links_user_id FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE SET NULL ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE signatory_links"
      ++ " ADD CONSTRAINT fk_signatory_links_company_id FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
