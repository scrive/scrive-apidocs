module Doc.Migrations where

import DB.Classes
import DB.Model
import Doc.Tables

addColumnToRecordInternalInsertionOrder :: Migration
addColumnToRecordInternalInsertionOrder =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "CREATE SEQUENCE signatory_links_internal_insert_order_seq"
      kRunRaw $ "ALTER TABLE signatory_links"
        ++ " ADD COLUMN internal_insert_order BIGINT NOT NULL DEFAULT nextval('signatory_links_internal_insert_order_seq')"
      return ()
  }

addDocumentIdIndexOnSignatoryLinks :: Migration
addDocumentIdIndexOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw $ "CREATE INDEX idx_signatory_links_document_id ON signatory_links(document_id)"
      return ()
  }

addNameColumnInSignatoryAttachments :: Migration
addNameColumnInSignatoryAttachments =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE signatory_attachments ADD COLUMN name TEXT NOT NULL DEFAULT ''"
  }

addCSVUploadDataFromDocumentToSignatoryLink :: Migration
addCSVUploadDataFromDocumentToSignatoryLink =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_links"
        ++ " ADD COLUMN csv_title TEXT NULL,"
        ++ " ADD COLUMN csv_contents TEXT NULL,"
        ++ " ADD COLUMN csv_signatory_index INTEGER NULL"
  }
