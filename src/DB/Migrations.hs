{-# LANGUAGE CPP #-}
module DB.Migrations (
    checkDBConsistency
  ) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Database.HDBC

import DB.Classes
import DB.Model
import DB.Versions
import DB.Utils
import qualified AppLogger as Log

import API.Service.Tables
import Company.Tables
#ifdef DOCUMENTS_IN_POSTGRES
import Doc.Tables
#endif
import User.Migrations
import User.Tables
import Stats.Tables
import Stats.Migrations
import File.Tables
import File.Model ()

migrationsList :: [Migration]
migrationsList = [addRegionToUserSettings
                 ,addServiceAndCompanyToStats
                 ,removeSystemServer]

tablesList :: [Table]
tablesList = [
    tableVersions
  , tableUsers
  , tableUserFriends
  , tableUserMailAPIs
  , tableUserInviteInfos
  , tableServices
  , tableCompanies
  , tableCompanyInvites
  , tableDocStatEvents
  , tableUserStatEvents
  , tableFiles
#ifdef DOCUMENTS_IN_POSTGRES
  , tableDocuments
  , tableSignatoryLinks
  -- this is not ready yet
  {-
  , tableAuthorAttachments
  , tableSignatoryAttachments
  -}
#endif
  ]

checkDBConsistency :: DB ()
checkDBConsistency = do
  (created, to_migration) <- checkTables
  forM_ created $ \table -> do
    Log.debug $ "Putting properties on table '" ++ tblName table ++ "'..."
    tblPutProperties table
  when (not $ null to_migration) $ do
    Log.debug "Running migrations..."
    migrate migrationsList to_migration
    Log.debug "Done."
    (_, to_migration_again) <- checkTables
    when (not $ null to_migration_again) $
      error $ "The following tables were not migrated to their latest versions: " ++ concatMap descNotMigrated to_migration_again
  where
    descNotMigrated (t, from) = "\n * " ++ tblName t ++ ", current version: " ++ show from ++ ", needed version: " ++ show (tblVersion t)
    checkTables = second catMaybes . partitionEithers <$> mapM checkTable tablesList
    checkTable table = do
      desc <- wrapDB $ \conn -> describeTable conn $ tblName table
      Log.debug $ "Checking table '" ++ tblName table ++ "'..."
      tvr <- tblCreateOrValidate table desc
      case tvr of
        TVRvalid -> do
          Log.debug "Table structure is valid, checking table version..."
          ver <- checkVersion
          if ver == tblVersion table
             then do
               Log.debug "Version of table in application matches database version."
               return $ Right Nothing
             else do
               Log.debug $ "Versions differ (application: " ++ show (tblVersion table) ++ ", database: " ++ show ver ++ "), scheduling for migration."
               return $ Right $ Just (table, ver)
        TVRcreated -> do
          Log.debug $ "Table created, writing version information..."
          wrapDB $ \conn -> do
            _ <- run conn "INSERT INTO table_versions (name, version) VALUES (?, ?)"
              [toSql $ tblName table, toSql $ tblVersion table]
            return ()
          _ <- checkTable table
          return $ Left table
        TVRinvalid -> do
          Log.debug $ "Table structure is invalid, checking version..."
          ver <- checkVersion
          if ver == tblVersion table
             then do
             Log.debug $ "Existing table: " ++ show desc
             error $ "Existing '" ++ tblName table ++ "' table structure is invalid"
             else do
               Log.debug "Table is outdated, scheduling for migration."
               return $ Right $ Just (table, ver)
      where
        checkVersion = wrapDB $ \conn -> do
          mver <- quickQuery' conn "SELECT version FROM table_versions WHERE name = ?"
            [toSql $ tblName table] >>= oneObjectReturnedGuard . join
          case mver of
            Just ver -> return $ fromSql ver
            _ -> error $ "No version information about table '" ++ tblName table ++ "' was found in database"
    migrate ms ts = forM_ ms $ \m -> forM_ ts $ \(t, from) -> do
      if tblName (mgrTable m) == tblName t && mgrFrom m >= from
         then do
           Log.debug $ "Migrating table '" ++ tblName t ++ "' from version " ++ show (mgrFrom m) ++ "..."
           mgrDo m
           wrapDB $ \conn -> do
             _ <- run conn "UPDATE table_versions SET version = ? WHERE name = ?"
               [toSql $ succ $ mgrFrom m, toSql $ tblName t]
             return ()
         else return ()
