module Doc.Migrations where

import Database.HDBC

import DB.Classes
import DB.Model
import Doc.Tables

addColumnToRecordInternalInsertionOrder :: Migration
addColumnToRecordInternalInsertionOrder =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 2
  , mgrDo = wrapDB $ \conn -> do
      _ <- runRaw conn "CREATE SEQUENCE signatory_links_internal_insert_order_seq"
      _ <- runRaw conn $ "ALTER TABLE signatory_links"
           ++ " ADD COLUMN internal_insert_order BIGINT NOT NULL DEFAULT nextval('signatory_links_internal_insert_order_seq')"
      return ()
  }

addNameColumnInSignatoryAttachments :: Migration
addNameColumnInSignatoryAttachments =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 1
  , mgrDo = wrapDB $ \conn -> do
      putStrLn "Migrating tableSignatoryAttachments"
      _ <- run conn "ALTER TABLE signatory_attachments ADD COLUMN name TEXT NOT NULL DEFAULT ''" []
      return ()
  }

addCSVUploadDataFromDocumentToSignatoryLink :: Migration
addCSVUploadDataFromDocumentToSignatoryLink =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 1
  , mgrDo = wrapDB $ \conn -> do
      putStrLn "Migrating tableSignatoryLinks"
      _ <- run conn ("ALTER TABLE signatory_links" ++
                     " ADD COLUMN csv_title TEXT NULL," ++
                     " ADD COLUMN csv_contents TEXT NULL," ++
                     " ADD COLUMN csv_signatory_index INTEGER NULL") []
      return ()
  }
