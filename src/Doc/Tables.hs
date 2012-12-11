module Doc.Tables where

import DB

tableDocuments :: Table
tableDocuments = tblTable {
    tblName = "documents"
  , tblVersion = 16
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("sealed_file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("title", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("status", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("error_text", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("type", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("process", SqlColDesc {colType = SqlSmallIntT, colNullable = Just True})
       , ("ctime", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("mtime", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("days_to_sign", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("timeout_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("invite_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("invite_ip", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("invite_text", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("trust_weaver_reference", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_title", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_contents", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_signatory_index", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("cancelation_reason", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("sharing", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("rejection_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("rejection_signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("rejection_reason", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("mail_footer", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("lang", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("authentication_method", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("delivery_method", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("api_callback_url", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("unsaved_draft", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE documents ("
          <> "  id BIGSERIAL"
          <> ", file_id BIGINT NULL"
          <> ", sealed_file_id BIGINT NULL"
          <> ", title TEXT NOT NULL"
          <> ", status SMALLINT NOT NULL"
          <> ", error_text TEXT NULL DEFAULT NULL"
          <> ", type SMALLINT NOT NULL"
          <> ", process SMALLINT NULL"
          <> ", ctime TIMESTAMPTZ NOT NULL"
          <> ", mtime TIMESTAMPTZ NOT NULL"
          <> ", days_to_sign INTEGER NOT NULL"
          <> ", timeout_time TIMESTAMPTZ NULL"
          <> ", invite_time TIMESTAMPTZ NULL"
          <> ", invite_ip INTEGER NULL"
          <> ", invite_text TEXT NOT NULL"
          <> ", trust_weaver_reference TEXT NULL"
          <> ", csv_title TEXT NULL"
          <> ", csv_contents TEXT NULL"
          <> ", csv_signatory_index INTEGER NULL"
          <> ", cancelation_reason TEXT NULL"
          <> ", sharing SMALLINT NOT NULL"
          <> ", rejection_time TIMESTAMPTZ NULL"
          <> ", rejection_signatory_link_id BIGINT NULL"
          <> ", rejection_reason TEXT NULL"
          <> ", mail_footer TEXT NULL"
          <> ", lang SMALLINT NOT NULL"
          <> ", authentication_method SMALLINT NOT NULL"
          <> ", delivery_method SMALLINT NOT NULL"
          <> ", api_callback_url TEXT NULL"
          <> ", unsaved_draft BOOL NOT NULL DEFAULT FALSE"
          <> ", CONSTRAINT pk_documents PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "file_id" "files" "id")
                     , (tblForeignKeyColumn "sealed_file_id" "files" "id") ]
  }

tableAuthorAttachments :: Table
tableAuthorAttachments = tblTable {
    tblName = "author_attachments"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE author_attachments ("
          <> "  file_id BIGINT NOT NULL"
          <> ", document_id BIGINT NOT NULL"
          <> ", CONSTRAINT pk_author_attachments PRIMARY KEY (file_id, document_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "file_id" "files" "id")
                     , (tblForeignKeyColumn "document_id" "documents" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableSignatoryAttachments :: Table
tableSignatoryAttachments = tblTable {
    tblName = "signatory_attachments"
  , tblVersion = 7
  , tblCreateOrValidate = \desc -> case desc of
      [  ("file_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("description", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE signatory_attachments "
          <> "( file_id BIGINT NULL"
          <> ", description TEXT NOT NULL"
          <> ", name TEXT NOT NULL"
          <> ", signatory_link_id BIGINT NOT NULL DEFAULT 0"
          <> ", CONSTRAINT pk_signatory_attachments PRIMARY KEY (signatory_link_id, name)"
          <> ")"
        return TVRcreated
      _ -> do
        return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "signatory_link_id" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "file_id" "files" "id")
                     , (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableSignatoryLinks :: Table
tableSignatoryLinks = tblTable {
    tblName = "signatory_links"
  , tblVersion = 15
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
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
       , ("deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("really_deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("csv_title", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_contents", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("csv_signatory_index", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("signinfo_ocsp_response", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("sign_redirect_url", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("is_author", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("is_partner", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE signatory_links"
          <> "( id BIGSERIAL"
          <> ", document_id BIGINT NOT NULL"
          <> ", user_id BIGINT NULL DEFAULT NULL"
          <> ", sign_order INTEGER NOT NULL DEFAULT 1"
          <> ", token BIGINT NOT NULL"
          <> ", sign_time TIMESTAMPTZ NULL DEFAULT NULL"
          <> ", sign_ip INTEGER NULL DEFAULT NULL"
          <> ", seen_time TIMESTAMPTZ NULL DEFAULT NULL"
          <> ", seen_ip INTEGER NULL DEFAULT NULL"
          <> ", read_invitation TIMESTAMPTZ NULL DEFAULT NULL"
          <> ", invitation_delivery_status SMALLINT NOT NULL DEFAULT 3"     -- this is Unknown
          <> ", signinfo_text TEXT NULL DEFAULT NULL"
          <> ", signinfo_signature TEXT NULL DEFAULT NULL"
          <> ", signinfo_certificate TEXT NULL DEFAULT NULL"
          <> ", signinfo_provider SMALLINT NULL DEFAULT NULL"
          <> ", signinfo_first_name_verified BOOL NULL DEFAULT NULL"
          <> ", signinfo_last_name_verified BOOL NULL DEFAULT NULL"
          <> ", signinfo_personal_number_verified BOOL NULL DEFAULT NULL"
          <> ", deleted BOOL NOT NULL DEFAULT false"
          <> ", really_deleted BOOL NOT NULL DEFAULT false"
          <> ", csv_title TEXT NULL"
          <> ", csv_contents TEXT NULL"
          <> ", csv_signatory_index INTEGER NULL"
          <> ", signinfo_ocsp_response VARCHAR NULL DEFAULT NULL"
          <> ", sign_redirect_url VARCHAR NULL DEFAULT NULL"
          <> ", is_author BOOL NOT NULL"
          <> ", is_partner BOOL NOT NULL"
          <> ", CONSTRAINT pk_signatory_links PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "user_id"
                 , tblIndexOnColumn "document_id" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "document_id" "documents" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "user_id" "users" "id")]
  }


tableDocumentTags :: Table
tableDocumentTags = tblTable {
    tblName = "document_tags"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> case desc of
      [ ("document_id", SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("name",        SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("value",       SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE document_tags"
                  <> "( document_id BIGINT NOT NULL"
                  <> ", name        TEXT   NOT NULL"
                  <> ", value       TEXT   NOT NULL"
                  <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "document_id" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "document_id" "documents" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableSignatoryLinkFields :: Table
tableSignatoryLinkFields = tblTable {
    tblName = "signatory_link_fields"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> case desc of
      [ ("id",                 SqlColDesc {colType = SqlBigIntT,   colNullable = Just False})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT,   colNullable = Just False})
       , ("type",              SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("custom_name",       SqlColDesc {colType = SqlVarCharT,  colNullable = Just False})
       , ("value",             SqlColDesc {colType = SqlVarCharT,  colNullable = Just False})
       , ("is_author_filled",  SqlColDesc {colType = SqlBitT,      colNullable = Just False})
       , ("placements",        SqlColDesc {colType = SqlVarCharT,  colNullable = Just False})
       , ("obligatory",        SqlColDesc {colType = SqlBitT,      colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE signatory_link_fields"
                  <> "( id                BIGSERIAL"
                  <> ", signatory_link_id BIGINT    NOT NULL"
                  <> ", type              SMALLINT  NOT NULL"
                  <> ", custom_name       TEXT      NOT NULL DEFAULT ''"
                  <> ", value             TEXT      NOT NULL DEFAULT ''"
                  <> ", is_author_filled  BOOL      NOT NULL DEFAULT FALSE"
                  <> ", placements        TEXT      NOT NULL DEFAULT ''"
                  <> ", obligatory        BOOL      NOT NULL DEFAULT TRUE"
                  <> ", CONSTRAINT pk_signatory_link_fields PRIMARY KEY (id)"
                  <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "signatory_link_id" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableSignatoryScreenshots :: Table
tableSignatoryScreenshots = tblTable {
    tblName = "signatory_screenshots"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",                SqlColDesc {colType = SqlBigIntT,              colNullable = Just False})
       , ("signatory_link_id", SqlColDesc {colType = SqlBigIntT,              colNullable = Just False})
       , ("type",              SqlColDesc {colType = SqlVarCharT,             colNullable = Just False})
       , ("time",              SqlColDesc {colType = SqlTimestampWithZoneT,   colNullable = Just False})
       , ("mimetype",          SqlColDesc {colType = SqlVarCharT,             colNullable = Just False})
       , ("image",             SqlColDesc {colType = SqlVarBinaryT,           colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE signatory_screenshots"
                  <> "( id                BIGSERIAL"
                  <> ", signatory_link_id BIGINT      NOT NULL"
                  <> ", type              TEXT        NOT NULL"
                  <> ", time              TIMESTAMPTZ NOT NULL"
                  <> ", mimetype          TEXT        NOT NULL"
                  <> ", image             BYTEA       NOT NULL"
                  <> ", CONSTRAINT pk_signatory_screenshots PRIMARY KEY (id)"
                  <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "signatory_link_id" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }
