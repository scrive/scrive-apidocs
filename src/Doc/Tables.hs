module Doc.Tables where

import DB

tableDocuments :: Table
tableDocuments = Table {
    tblName = "documents"
  , tblVersion = 5
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("service_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("sealed_file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("title", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("status", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("error_text", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("type", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("process", SqlColDesc {colType = SqlSmallIntT, colNullable = Just True})
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
       , ("csv_contents", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_signatory_index", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("cancelation_reason", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("sharing", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("rejection_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("rejection_signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("rejection_reason", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("mail_footer", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("region", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE documents ("
          ++ "  id BIGINT NOT NULL"
          ++ ", service_id TEXT NULL"
          ++ ", file_id BIGINT NULL"
          ++ ", sealed_file_id BIGINT NULL"
          ++ ", title TEXT NOT NULL"
          ++ ", status SMALLINT NOT NULL"
          ++ ", error_text TEXT NULL DEFAULT NULL"
          ++ ", type SMALLINT NOT NULL"
          ++ ", process SMALLINT NULL"
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
          ++ ", csv_contents TEXT NULL"
          ++ ", csv_signatory_index INTEGER NULL"
          ++ ", cancelation_reason TEXT NULL"
          ++ ", sharing SMALLINT NOT NULL"
          ++ ", rejection_time TIMESTAMPTZ NULL"
          ++ ", rejection_signatory_link_id BIGINT NULL"
          ++ ", rejection_reason TEXT NULL"
          ++ ", mail_footer TEXT NULL"
          ++ ", region SMALLINT NOT NULL"
          ++ ", deleted BOOL NOT NULL"
          ++ ", CONSTRAINT pk_documents PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "CREATE INDEX idx_documents_service_id ON documents(service_id)"
    kRunRaw $ "ALTER TABLE documents"
      ++ " ADD CONSTRAINT fk_documents_services FOREIGN KEY(service_id)"
      ++ " REFERENCES services(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE documents"
      ++ " ADD CONSTRAINT fk_documents_file_id FOREIGN KEY(file_id)"
      ++ " REFERENCES files(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE documents"
      ++ " ADD CONSTRAINT fk_documents_sealed_file_id FOREIGN KEY(sealed_file_id)"
      ++ " REFERENCES files(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
      -- create the sequence
    kRunRaw $ "CREATE SEQUENCE documents_id_seq"
    kRunRaw $ "SELECT setval('documents_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM documents))"
    kRunRaw $ "ALTER TABLE documents ALTER id SET DEFAULT nextval('documents_id_seq')"
    return ()
  }


tableAuthorAttachments :: Table
tableAuthorAttachments = Table {
    tblName = "author_attachments"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE author_attachments ("
          ++ "  file_id BIGINT NOT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_author_attachments PRIMARY KEY (file_id, document_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE author_attachments"
      ++ " ADD CONSTRAINT fk_author_attachments_files FOREIGN KEY(file_id)"
      ++ " REFERENCES files(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE author_attachments"
      ++ " ADD CONSTRAINT fk_author_attachments_documents FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableSignatoryAttachments :: Table
tableSignatoryAttachments = Table {
    tblName = "signatory_attachments"
  , tblVersion = 3
  , tblCreateOrValidate = \desc -> case desc of
      [  ("file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("description", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE signatory_attachments "
          ++ "( file_id BIGINT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", description TEXT NOT NULL"
          ++ ", name TEXT NOT NULL"
          ++ ", signatory_link_id BIGINT NOT NULL DEFAULT 0"
          ++ ", CONSTRAINT pk_signatory_attachments PRIMARY KEY (document_id, signatory_link_id, name)"
          ++ ")"
        return TVRcreated
      _ -> do
        return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "CREATE INDEX idx_signatory_attachments_document_id ON signatory_attachments(document_id)"
    kRunRaw $ "CREATE INDEX idx_signatory_attachments_signatory_link_id ON signatory_attachments(signatory_link_id)"
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD CONSTRAINT fk_signatory_attachments_files FOREIGN KEY(file_id)"
      ++ " REFERENCES files(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD CONSTRAINT fk_signatory_attachments_documents FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id)"
      ++ " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableSignatoryLinks :: Table
tableSignatoryLinks = Table {
    tblName = "signatory_links"
  , tblVersion = 6
  , tblCreateOrValidate = \desc -> case desc of
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
       , ("csv_title", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_contents", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_signatory_index", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("internal_insert_order", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE SEQUENCE signatory_links_internal_insert_order_seq"
        kRunRaw $ "CREATE TABLE signatory_links"
          ++ "( id BIGINT NOT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", user_id BIGINT NULL DEFAULT NULL"
          ++ ", company_id BIGINT NULL DEFAULT NULL"
          ++ ", fields TEXT NOT NULL DEFAULT NULL"
          ++ ", sign_order INTEGER NOT NULL DEFAULT 1"
          ++ ", token BIGINT NOT NULL"
          ++ ", sign_time TIMESTAMPTZ NULL DEFAULT NULL"
          ++ ", sign_ip INTEGER NULL DEFAULT NULL"
          ++ ", seen_time TIMESTAMPTZ NULL DEFAULT NULL"
          ++ ", seen_ip INTEGER NULL DEFAULT NULL"
          ++ ", read_invitation TIMESTAMPTZ NULL DEFAULT NULL"
          ++ ", invitation_delivery_status SMALLINT NOT NULL DEFAULT 3"     -- this is Unknown
          ++ ", signinfo_text TEXT NULL DEFAULT NULL"
          ++ ", signinfo_signature TEXT NULL DEFAULT NULL"
          ++ ", signinfo_certificate TEXT NULL DEFAULT NULL"
          ++ ", signinfo_provider SMALLINT NULL DEFAULT NULL"
          ++ ", signinfo_first_name_verified BOOL NULL DEFAULT NULL"
          ++ ", signinfo_last_name_verified BOOL NULL DEFAULT NULL"
          ++ ", signinfo_personal_number_verified BOOL NULL DEFAULT NULL"
          ++ ", roles INTEGER NOT NULL"
          ++ ", deleted BOOL NOT NULL DEFAULT false"
          ++ ", really_deleted BOOL NOT NULL DEFAULT false"
          ++ ", csv_title TEXT NULL"
          ++ ", csv_contents TEXT NULL"
          ++ ", csv_signatory_index INTEGER NULL"
          ++ ", internal_insert_order BIGINT NOT NULL DEFAULT nextval('signatory_links_internal_insert_order_seq')"
          ++ ", CONSTRAINT pk_signatory_links PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "CREATE INDEX idx_signatory_links_user_id ON signatory_links(user_id)"
    kRunRaw $ "CREATE INDEX idx_signatory_links_company_id ON signatory_links(company_id)"
    kRunRaw $ "CREATE INDEX idx_signatory_links_document_id ON signatory_links(document_id)"
    kRunRaw $ "ALTER TABLE signatory_links"
      ++ " ADD CONSTRAINT fk_signatory_links_document_id FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE signatory_links"
      ++ " ADD CONSTRAINT fk_signatory_links_user_id FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE SET NULL ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE signatory_links"
      ++ " ADD CONSTRAINT fk_signatory_links_company_id FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "CREATE SEQUENCE signatory_links_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
    kRunRaw $ "SELECT setval('signatory_links_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM signatory_links))"
      -- and finally attach serial default value to files.id
    kRunRaw $ "ALTER TABLE signatory_links ALTER id SET DEFAULT nextval('signatory_links_id_seq')"
  }


tableDocumentTags :: Table
tableDocumentTags = Table {
    tblName = "document_tags"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [ ("document_id", SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("name",        SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("value",       SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE document_tags"
                  ++ "( document_id BIGINT NOT NULL"
                  ++ ", name        TEXT   NOT NULL"
                  ++ ", value       TEXT   NOT NULL"
                  ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "CREATE INDEX idx_document_tags_document_id ON document_tags(document_id)"
  }
