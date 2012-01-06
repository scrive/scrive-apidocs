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

changeIPAddressTypeInDocuments :: Migration
changeIPAddressTypeInDocuments =
  Migration
    { mgrTable = tableDocuments
    , mgrFrom = 1
    , mgrDo = wrapDB $ \conn -> do
      putStrLn "Migrating tableDocuments"
      _ <- run conn "ALTER TABLE documents ALTER COLUMN invite_ip TYPE BIGINT" []
      return ()
  }

changeIPAddressTypeInSignatoryLinks :: Migration
changeIPAddressTypeInSignatoryLinks =
  Migration
    { mgrTable = tableSignatoryLinks
    , mgrFrom = 1
    , mgrDo = wrapDB $ \conn -> do
      putStrLn "Migrating SignatoryLinks"
      _ <- run conn "ALTER TABLE signatory_links ALTER COLUMN sign_ip TYPE BIGINT" []
      _ <- run conn "ALTER TABLE signatory_links ALTER COLUMN seen_ip TYPE BIGINT" []
      return ()
  }

