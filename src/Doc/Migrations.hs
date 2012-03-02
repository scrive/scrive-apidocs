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
