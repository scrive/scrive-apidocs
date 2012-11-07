{-# LANGUAGE OverloadedStrings #-}
module DB.Checks (
    performDBChecks
  ) where

import Control.Arrow
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Database.HDBC

import DB.Core
import DB.Env
import DB.Functions
import DB.Model
import DB.SQL
import DB.Utils
import DB.Versions

-- | Runs all checks on a database
performDBChecks :: MonadDB m => (String -> m ()) -> [Table] -> [Migration m] -> m ()
performDBChecks logger tables migrations = runDBEnv $ do
  let liftedLogger = lift . logger
  set <- setByteaOutput liftedLogger
  when set $ do
    lift dbCommit
    error $ "Bytea_output was changed to 'hex'. Restart application so the change is visible."
  checkDBTimeZone liftedLogger
  checkDBConsistency liftedLogger (tableVersions : tables) migrations
  -- everything is OK, commit changes
  lift dbCommit
  return ()

-- |  Checks whether database returns timestamps in UTC
checkDBTimeZone :: MonadDB m => (String -> DBEnv m ()) -> DBEnv m ()
checkDBTimeZone logger = do
  Just dbname <- getOne "SELECT current_catalog"
  logger $ "Setting '" ++ dbname ++ "' database to return timestamps in UTC"
  _ <- kRun $ SQL ("ALTER DATABASE \"" ++ dbname ++ "\" SET TIMEZONE = 'UTC'") []
  return ()

setByteaOutput :: MonadDB m => (String -> DBEnv m ()) -> DBEnv m Bool
setByteaOutput logger = do
  Just dbname <- getOne "SELECT current_catalog"
  Just bytea_output <- getOne "SHOW bytea_output"
  if bytea_output /= ("hex" :: String)
    then do
      logger $ "Setting bytea_output to 'hex'..."
      kRunRaw $ "ALTER DATABASE \"" ++ dbname ++ "\" SET bytea_output = 'hex'"
      return True
    else return False

-- | Checks whether database is consistent (performs migrations if necessary)
checkDBConsistency :: MonadDB m => (String -> DBEnv m ()) -> [Table] -> [Migration m] -> DBEnv m ()
checkDBConsistency logger tables migrations = do
  (created, to_migration) <- checkTables
  when (not $ null to_migration) $ do
    logger "Running migrations..."
    migrate migrations to_migration
    logger "Done."
    (_, to_migration_again) <- checkTables
    when (not $ null to_migration_again) $
      error $ "The following tables were not migrated to their latest versions: " ++ concatMap descNotMigrated to_migration_again
  forM_ created $ \table -> do
    logger $ "Putting properties on table '" ++ tblName table ++ "'..."
    tblPutProperties table
  where
    descNotMigrated (t, from) = "\n * " ++ tblName t ++ ", current version: " ++ show from ++ ", needed version: " ++ show (tblVersion t)

    checkTables = (second catMaybes . partitionEithers) `liftM` mapM checkTable tables
    checkTable table = do
      desc <- kDescribeTable $ tblName table
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
          kPrepare "INSERT INTO table_versions (name, version) VALUES (?, ?)"
          _ <- kExecute [toSql $ tblName table, toSql $ tblVersion table]
          _ <- checkTable table
          return $ Left table
        TVRinvalid -> do
          logger $ "Table structure is invalid, checking version..."
          ver <- checkVersion table
          if ver == tblVersion table
             then do
               logger $ show desc
               error $ "Existing '" ++ tblName table ++ "' table structure is invalid"
             else do
               logger "Table is outdated, scheduling for migration."
               return $ Right $ Just (table, ver)

    checkVersion table = do
      mver <- getOne $ SQL "SELECT version FROM table_versions WHERE name = ?" [toSql $ tblName table]
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
           kPrepare "UPDATE table_versions SET version = ? WHERE name = ?"
           _ <- kExecute [toSql $ succ $ mgrFrom m, toSql $ tblName t]
           return ()
         else return ()
