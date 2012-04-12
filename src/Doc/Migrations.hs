module Doc.Migrations where

import Control.Monad
import Data.Int
import Database.HDBC
import Text.JSON

import DB.Classes
import DB.Fetcher2
import DB.Model
import DB.Utils
import Doc.Tables
import qualified Log
import Doc.DocumentID
import Doc.DocStateData()
import qualified Data.ByteString as BS

moveDocumentTagsFromDocumentsTableToDocumentTagsTable :: Migration
moveDocumentTagsFromDocumentsTableToDocumentTagsTable = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 4
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, tags FROM documents WHERE tags <> '' AND tags <> '[]'" [];
    values <- foldDB fetch []
    forM_ values $ \(docid, tags) -> do
      forM_ tags $ \tag -> do
        let Just (JSString tagname) = lookup "tagname" tag
            Just (JSString tagvalue) = lookup "tagvalue" tag
        kRun $ mkSQL INSERT tableDocumentTags
           [ sql "name" $ fromJSString tagname
           , sql "value" $ fromJSString tagvalue
           , sql "document_id" docid
           ]
      return ()
    kRunRaw $ "ALTER TABLE documents DROP COLUMN tags"
  }
  where
    fetch acc docid tagsstr = (docid :: Int64, tags) : acc
      where
        Ok (JSArray arr) = decode tagsstr
        fromJSValue (JSObject obj) = fromJSObject obj
        fromJSValue x = 
          error $ "moveDocumentTagsFromDocumentsTableToDocumentTagsTable: expected {tagname:'',tagvalue:''}, got: " ++ encode x
        tags = map fromJSValue arr

updateDocumentStatusAfterRemovingAwaitingAuthor :: Migration
updateDocumentStatusAfterRemovingAwaitingAuthor = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 3
  , mgrDo = do
    -- change AwaitingAuthor to Pending
    kRunRaw "UPDATE documents SET status = 2 WHERE status = 7"
    -- update DocumentError so it has proper value
    kRunRaw "UPDATE documents SET status = 7 WHERE status = 8"
  }

removeOldSignatoryLinkIDFromCancelationReason :: Migration
removeOldSignatoryLinkIDFromCancelationReason = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 1
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, cancelation_reason FROM documents WHERE cancelation_reason LIKE ?" [toSql "{\"ELegDataMismatch%"]
    values <- foldDB fetch []
    forM_ values $ \(slid, params) -> do
      let (x : JSObject link : xs) = params
          [("unSignatoryLinkID", newlink)] = fromJSObject link
          newparams = toJSObject [("ELegDataMismatch", x : newlink : xs)]
      1 <- kRun $ SQL "UPDATE documents SET cancelation_reason = ? WHERE id = ?"
        [toSql $ encode newparams, toSql slid]
      return ()
  }
  where
    fetch acc slid reason = (slid :: Int64, params) : acc
      where
        Ok (JSObject o) = decode reason
        [("ELegDataMismatch", JSArray params)] = fromJSObject o

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

addIdSerialOnSignatoryLinks :: Migration
addIdSerialOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 4
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE signatory_links_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('signatory_links_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM signatory_links))" []
      Log.debug $ "Table signatory_links has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE signatory_links ALTER id SET DEFAULT nextval('signatory_links_id_seq')"
      return ()
  }

addIdSerialOnDocuments :: Migration
addIdSerialOnDocuments =
  Migration {
    mgrTable = tableDocuments
  , mgrFrom = 2
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE documents_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('documents_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM documents))" []
      Log.debug $ "Table documents has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE documents ALTER id SET DEFAULT nextval('documents_id_seq')"
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
    -- set the new column signatory_link_id from signatory_links that have the same email and document_id
    kRunRaw $ "UPDATE signatory_attachments "
      ++ "SET signatory_link_id = sl.id "
      ++ "FROM signatory_links sl "
      ++ "WHERE sl.document_id = signatory_attachments.document_id "
      ++ "AND regexp_replace(sl.fields, '^.*EmailFT\",\"sfValue\":\"([a-zA-Z0-9@-_.]+)\".*$', E'\\\\1') = signatory_attachments.email"
    kRunRaw $ "ALTER TABLE signatory_attachments DROP CONSTRAINT pk_signatory_attachments"
    -- delete attachments which have emails and document_id that don't exist in signatory_links  
    logAndDeleteBadAttachments
    kRunRaw $ "ALTER TABLE signatory_attachments DROP COLUMN email"
    kRunRaw $ "ALTER TABLE signatory_attachments ADD CONSTRAINT pk_signatory_attachments PRIMARY KEY (document_id, signatory_link_id, name)"
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id, document_id)"
      ++ " REFERENCES signatory_links(id, document_id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "CREATE INDEX idx_signatory_attachments_signatory_link_id ON signatory_attachments(signatory_link_id)"
  }
  where
    logAndDeleteBadAttachments :: MonadDB m => DBEnv m ()
    logAndDeleteBadAttachments = do 
      kRunRaw $ "SELECT document_id, name, email, description FROM signatory_attachments WHERE signatory_link_id = 0"
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

