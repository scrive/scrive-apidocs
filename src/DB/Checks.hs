module DB.Checks (
    performDBChecks
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Data.Time.LocalTime
import Database.HDBC

import DB.Classes
import DB.Model
import DB.Utils

-- | Runs all checks on a database
performDBChecks :: (String -> DB ()) -> [Table] -> [Migration] -> DB ()
performDBChecks logger tables migrations = do
  checkDBTimeZone logger
  checkDBConsistency logger tables migrations

-- | Checks whether database returns timestamps in UTC
checkDBTimeZone :: (String -> DB ()) -> DB ()
checkDBTimeZone logger = do
  logger "Checking whether database returns timestamps in UTC..."
  tz <- maybe (error "'SELECT now()' returned nothing") zonedTimeZone <$> getOne "SELECT now()" []
  if timeZoneMinutes tz == 0
     then return ()
     else error $ "Database returns timestamps using time zone " ++ show tz ++ ". Execute query \"ALTER DATABASE your_database SET TIMEZONE = 'UTC'\" and try again."

-- | Checks whether database is consistent (performs migrations if necessary)
checkDBConsistency :: (String -> DB ()) -> [Table] -> [Migration] -> DB ()
checkDBConsistency logger tables migrations = do
  (created, to_migration) <- checkTables tables
  forM_ created $ \table -> do
    logger $ "Putting properties on table '" ++ tblName table ++ "'..."
    tblPutProperties table
  when (not $ null to_migration) $ do
    logger "Running migrations..."
    migrate migrations to_migration
    logger "Done."
    (_, to_migration_again) <- checkTables tables
    when (not $ null to_migration_again) $
      error $ "The following tables were not migrated to their latest versions: " ++ concatMap descNotMigrated to_migration_again
  where
    descNotMigrated (t, from) = "\n * " ++ tblName t ++ ", current version: " ++ show from ++ ", needed version: " ++ show (tblVersion t)

    checkTables tables = second catMaybes . partitionEithers <$> mapM checkTable tables
    checkTable table = do
      desc <- wrapDB $ \conn -> describeTable conn $ tblName table
      logger $ "Checking table '" ++ tblName table ++ "'..."
      tvr <- tblCreateOrValidate table desc
      case tvr of
        TVRvalid -> do
          logger "Table structure is valid, checking table version..."
          ver <- checkVersion table
          if ver == tblVersion table
             then do
               logger "Version of table in application matches database version."
               return $ Right Nothing
             else do
               logger $ "Versions differ (application: " ++ show (tblVersion table) ++ ", database: " ++ show ver ++ "), scheduling for migration."
               return $ Right $ Just (table, ver)
        TVRcreated -> do
          logger $ "Table created, writing version information..."
          wrapDB $ \conn -> do
            _ <- run conn "INSERT INTO table_versions (name, version) VALUES (?, ?)"
              [toSql $ tblName table, toSql $ tblVersion table]
            return ()
          _ <- checkTable table
          return $ Left table
        TVRinvalid -> do
          logger $ "Table structure is invalid, checking version..."
          ver <- checkVersion table
          if ver == tblVersion table
             then do
               error $ "Existing '" ++ tblName table ++ "' table structure is invalid"
             else do
               logger "Table is outdated, scheduling for migration."
               return $ Right $ Just (table, ver)

    checkVersion table = do
      mver <- getOne "SELECT version FROM table_versions WHERE name = ?" [toSql $ tblName table]
      case mver of
        Just ver -> return ver
        _ -> error $ "No version information about table '" ++ tblName table ++ "' was found in database"

    migrate ms ts = forM_ ms $ \m -> forM_ ts $ \(t, from) -> do
      if tblName (mgrTable m) == tblName t && mgrFrom m >= from
         then do
           logger $ "Migrating table '" ++ tblName t ++ "' from version " ++ show (mgrFrom m) ++ "..."
           ver <- checkVersion $ mgrTable m
           when (ver /= mgrFrom m) $
             error $ "Migration can't be performed because current table version (" ++ show ver ++ ") doesn't match parameter mgrFrom of next migration to be run (" ++ show (mgrFrom m) ++ "). Make sure that migrations were put in migrationsList in correct order."
           mgrDo m
           wrapDB $ \conn -> do
             _ <- run conn "UPDATE table_versions SET version = ? WHERE name = ?"
               [toSql $ succ $ mgrFrom m, toSql $ tblName t]
             return ()
         else return ()
