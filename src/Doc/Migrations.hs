module Doc.Migrations where

import DB.Classes
import DB.Model
import Doc.Tables
import DB.Fetcher2
import Log
import Doc.DocumentID
import Doc.DocStateData()
import qualified Data.ByteString as BS

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

addSignatoryLinkIdToSignatoryAttachment :: Migration
addSignatoryLinkIdToSignatoryAttachment =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_attachments"
        ++ " ADD COLUMN signatory_link_id BIGINT NOT NULL DEFAULT 0"
      kRunRaw $ "ALTER TABLE signatory_attachments"
        ++ " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id, document_id)"
        ++ " REFERENCES signatory_links(id, document_id) ON DELETE CASCADE ON UPDATE RESTRICT"
        ++ " DEFERRABLE INITIALLY IMMEDIATE"
      kRunRaw $ "UPDATE signatory_attachments "
        ++ "SET signatory_link_id = sl.id "
        ++ "FROM signatory_links sl "
        ++ "WHERE sl.document_id = signatory_attachments.document_id "
        ++ "AND regexp_replace(sl.fields, '^.*EmailFT\",\"sfValue\":\"([a-zA-Z0-9@-_.]+)\".*$', E'\\\\1') = signatory_attachments.email"
      kRunRaw $ "ALTER TABLE signatory_attachments DROP CONSTRAINT pk_signatory_attachments"
      kRunRaw $ "SELECT document_id, name, email, description FROM signatory_attachments WHERE signatory_link_id = 0"
      -- delete attachments which have emails and document_id that don't exist in signatory_links  
      logAndDeleteBadAttachments
      kRunRaw $ "ALTER TABLE signatory_attachments DROP COLUMN email"
      kRunRaw $ "ALTER TABLE signatory_attachments ADD CONSTRAINT pk_signatory_attachments PRIMARY KEY (document_id, signatory_link_id, name)"
      kRunRaw $ "CREATE INDEX idx_signatory_attachments_signatory_link_id ON signatory_attachments(signatory_link_id)"
  }

logAndDeleteBadAttachments :: DB ()
logAndDeleteBadAttachments = do 
    atts <- foldDB decoder []
    kRunRaw $ "DELETE FROM signatory_attachments WHERE signatory_link_id = 0"
    mapM_ (\(d, n, e, s) -> 
        Log.debug $ "Deleted bad attachment: document_id = " ++ show d
                 ++ ", name = " ++ show n
                 ++ ", email = " ++ show e
                 ++ ", description = " ++ show s) atts
    return ()
  where
    decoder :: [(DocumentID, BS.ByteString, BS.ByteString, BS.ByteString)] -> DocumentID -> BS.ByteString -> BS.ByteString -> BS.ByteString -> [(DocumentID, BS.ByteString, BS.ByteString, BS.ByteString)]
    decoder acc docid name email desc = (docid, name, email, desc) : acc

