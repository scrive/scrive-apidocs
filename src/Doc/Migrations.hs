module Doc.Migrations where

import Database.HDBC 

import DB.Classes
import DB.Model
import Doc.Tables


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
