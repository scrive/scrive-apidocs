{-# LANGUAGE ExtendedDefaultRules #-}
module DB.Checks (
    performDBChecks
  ) where

import Control.Arrow (second)
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Data.Monoid
import qualified Data.List as L
import Database.HDBC

import DB.Core
import DB.Env
import DB.Functions
import DB.Fetcher
import DB.Model
import DB.SQL
import DB.SQL2
import DB.Utils
import DB.Versions

default (SQL)

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

-- | Return SQL fragment of current catalog within quotes
currentCatalog :: MonadDB m => DBEnv m RawSQL
currentCatalog = do
  Just dbname <- getOne "SELECT current_catalog"
  return $ unsafeFromString $ "\"" ++ dbname ++ "\""

-- |  Checks whether database returns timestamps in UTC
checkDBTimeZone :: MonadDB m => (String -> DBEnv m ()) -> DBEnv m ()
checkDBTimeZone logger = do
  dbname <- currentCatalog
  logger $ "Setting " ++ unRawSQL dbname ++ " database to return timestamps in UTC"
  kRunRaw $ "ALTER DATABASE " <> dbname <> " SET TIMEZONE = 'UTC'"
  return ()

setByteaOutput :: MonadDB m => (String -> DBEnv m ()) -> DBEnv m Bool
setByteaOutput logger = do
  dbname <- currentCatalog
  Just bytea_output <- getOne "SHOW bytea_output"
  if bytea_output /= ("hex" :: String)
    then do
      logger $ "Setting bytea_output to 'hex'..."
      kRunRaw $ "ALTER DATABASE " <> dbname <> " SET bytea_output = 'hex'"
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
    logger $ "Putting properties on table '" ++ tblNameString table ++ "'..."
    tblPutProperties table

  forM_ tables $ \table -> do
    logger $ "Ensuring indexes on table '" ++ tblNameString table ++ "'..."
    checkIndexes table

    logger $ "Ensuring foreign keys on table '" ++ tblNameString table ++ "'..."
    checkForeignKeys table

  where
    tblNameString = unRawSQL . tblName
    descNotMigrated (t, from) = "\n * " ++ tblNameString t ++ ", current version: " ++ show from ++ ", needed version: " ++ show (tblVersion t)

    checkTables = (second catMaybes . partitionEithers) `liftM` mapM checkTable tables
    checkTable table = do
      desc <- kDescribeTable $ tblName table
      logger $ "Checking table '" ++ tblNameString table ++ "'..."
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
               error $ "Existing '" ++ tblNameString table ++ "' table structure is invalid"
             else do
               logger "Table is outdated, scheduling for migration."
               return $ Right $ Just (table, ver)

    checkVersion table = do
      mver <- getOne $ SQL "SELECT version FROM table_versions WHERE name = ?" [toSql $ tblName table]
      case mver of
        Just ver -> return ver
        _ -> error $ "No version information about table '" ++ tblNameString table ++ "' was found in database"

    migrate ms ts = forM_ ms $ \m -> forM_ ts $ \(t, from) -> do
      if tblName (mgrTable m) == tblName t && mgrFrom m >= from
         then do
           logger $ "Migrating table '" ++ tblNameString t ++ "' from version " ++ show (mgrFrom m) ++ "..."
           ver <- checkVersion $ mgrTable m
           when (ver /= mgrFrom m) $
             error $ "Migration can't be performed because current table version (" ++ show ver ++ ") doesn't match parameter mgrFrom of next migration to be run (" ++ show (mgrFrom m) ++ "). Make sure that migrations were put in migrationsList in correct order."
           mgrDo m
           kPrepare "UPDATE table_versions SET version = ? WHERE name = ?"
           _ <- kExecute [toSql $ succ $ mgrFrom m, toSql $ tblName t]
           return ()
         else return ()

    checkIndexes table = do
      let requested' = L.nub (tblIndexes table ++ map (tblIndexOnColumns . fkColumns) (tblForeignKeys table))
          mkName index = "idx_" <> tblName table <> "_" <> mconcat (L.intersperse "_" (tblIndexColumns index))
          requested = map (\ss -> (mkName ss, ss)) requested'

      -- This is PostgreSQL specific. We query database catalogs to
      -- know which indexes are present on a table.  We also get
      -- descriptions of columns of table for particular index.
      --
      -- We ignore all indexes that were implicitely created by
      -- PostgreSQL as support for UNIQUE or PRIMARY KEY
      -- constraints. Those belong to respective columns and
      -- constraints anyway so we do not have to manage them.
      --
      -- This method ignores all specila date on index: partiality,
      -- uniqueness, expressions. Support for advanced constructs is
      -- left for later implementation.
      kRun_ $ sqlSelect "" $ do
        sqlResult "pg_index_class.relname"
        sqlResult "pg_attribute.attname"
        sqlFrom "(SELECT pg_index.*, generate_subscripts(pg_index.indkey,1) AS indkey1 FROM pg_index) AS pg_index1"
        sqlJoinOn "pg_class AS pg_index_class" "pg_index_class.oid = pg_index1.indexrelid"
        sqlJoinOn "pg_class AS pg_table_class" "pg_table_class.oid = pg_index1.indrelid"
        sqlJoinOn "pg_attribute" "pg_table_class.oid = pg_attribute.attrelid AND pg_attribute.attnum = pg_index1.indkey[pg_index1.indkey1]"
        sqlWhereNotExists $ sqlSelect "pg_constraint" $ do
          sqlWhere "pg_constraint.conindid = pg_index1.indexrelid"
        sqlWhereEq "pg_table_class.relname" (tblName table)
        sqlOrderBy "pg_index_class.relname"
        sqlOrderBy "pg_index1.indkey1"

      -- Here we use unsafeFromString a couple of times. In general we
      -- do not want to have Convertible SQL instance for RawSQL
      -- because our database can store different strings. Just at
      -- this place in code we are sure of having a string name that
      -- can be used for SQL string substitution.
      let fetchNamedIndex ((idxname1,index):acc) idxname column | unsafeFromString idxname==idxname1 =
            (idxname1, index { tblIndexColumns = tblIndexColumns index ++ [unsafeFromString column]}) : acc
          fetchNamedIndex acc idxname column =
            (unsafeFromString idxname, TableIndex { tblIndexColumns = [unsafeFromString column] }) : acc

      present <- foldDB fetchNamedIndex []

      -- At this point we have 'present' that describes present
      -- indexes and 'requested' that describes what is requested.
      --
      -- To sync these two we need to go through present list and drop
      -- everything that is not structurally equivalent to anything on
      -- requested lists. Go through requested list and add anything
      -- that is not structurally equivalent to anything present.
      --
      -- Wrong names might happen because of table renames. We would
      -- need to do renames in that case. Ignored for now.
      --
      -- We ignore the possibiliy of duplicate indexes that have
      -- same structure.
      --
      -- Indexes also can have some more structure, like be partially
      -- defined or be defined on expressions instead of columns. For
      -- now we will ignore such indexes.
      --
      -- We also ignore indexes that are created for constraints like
      -- UNIQUE or PRIMARY KEY constraints.
      --
      let toDrop = filter shouldDrop present
          shouldDrop index = not (any (structurallySame index) requested)
          toAdd = filter shouldAdd requested
          shouldAdd index = not (any (structurallySame index) present)
          structurallySame (_name1,index1) (_name2,index2) =
            tblIndexColumns index1 == tblIndexColumns index2

      forM_ toDrop $ \(name,_def) -> do
        logger $ "   dropping index '" ++ unRawSQL name ++ "'"
        kRun_ $ "DROP INDEX" <+> raw name

      forM_ toAdd $ \(name,index) -> do
        logger $ "   adding index '" ++ unRawSQL name ++ "'"
        kRun_ $ "CREATE INDEX" <+> raw name
                   <+> "ON" <+> raw (tblName table)
                   <+> "(" <+> intersperse "," (map raw (tblIndexColumns index)) <+> ")"

      return ()

    checkForeignKeys table = do
      let fkRequested = tblForeignKeys table

      kRun_ $ sqlSelect "" $ do
        sqlResult "pg_constraint.conname"
        -- sqlResult "pg_constraint.idx"
        -- sqlResult "pg_class.relname"
        sqlResult "pg_attribute.attname"
        sqlResult "pg_fclass.relname"
        sqlResult "pg_fattribute.attname"
        sqlResult "pg_constraint.confupdtype AS upd"
        sqlResult "pg_constraint.confdeltype AS del"
        sqlResult "pg_constraint.condeferrable AS deferrable"
        sqlResult "pg_constraint.condeferred AS deferred"
        sqlFrom "(SELECT pg_catalog.pg_constraint.*, generate_subscripts(pg_constraint.conkey,1) AS idx FROM pg_constraint) AS pg_constraint"
        sqlJoinOn "pg_class" "pg_class.oid = pg_constraint.conrelid"
        sqlJoinOn "pg_class AS pg_fclass" "pg_fclass.oid = pg_constraint.confrelid"
        sqlJoinOn "pg_attribute" "pg_class.oid = pg_attribute.attrelid AND pg_attribute.attnum = pg_constraint.conkey[pg_constraint.idx]"
        sqlJoinOn "pg_attribute AS pg_fattribute" "pg_fclass.oid = pg_fattribute.attrelid AND pg_fattribute.attnum = pg_constraint.confkey[pg_constraint.idx]"
        sqlWhereEq "pg_constraint.contype" ("f" :: String)
        sqlWhereEq "pg_class.relname" (tblName table)
        sqlOrderBy "pg_constraint.conname"
        sqlOrderBy "pg_constraint.idx"

      let typeToAction code =
            case code of
              "a" -> ForeignKeyNoAction
              "r" -> ForeignKeyRestrict
              "c" -> ForeignKeyCascade
              "n" -> ForeignKeySetNull
              "d" -> ForeignKeySetDefault
              x   -> error $ "Invalid foreign key action code '" ++ x ++ "'"

      let fetchNamedForeignKey ((constraint_name1,fk):acc) constraint_name
                                   column_name
                                   _reftable_name refcolumn_name
                                   _update_type _delete_type
                                   _deferrable _deferred | unsafeFromString constraint_name == constraint_name1
                                   = ( constraint_name1
                                     , fk
                                      { fkColumns    = fkColumns fk ++ [unsafeFromString column_name]
                                      , fkRefColumns = fkRefColumns fk ++ [unsafeFromString refcolumn_name]
                                      }) : acc
          fetchNamedForeignKey acc constraint_name
                                   column_name
                                   reftable_name refcolumn_name
                                   update_type delete_type
                                   deferrable deferred
                                   = ( unsafeFromString constraint_name
                                     , ForeignKey
                                      { fkColumns    = [unsafeFromString column_name]
                                      , fkRefTable   = unsafeFromString reftable_name
                                      , fkRefColumns = [unsafeFromString refcolumn_name]
                                      , fkOnUpdate   = typeToAction update_type
                                      , fkOnDelete   = typeToAction delete_type
                                      , fkDeferrable = deferrable
                                      , fkDeferred   = deferred
                                      }) : acc

      fkPresent <- foldDB fetchNamedForeignKey []

      -- At this point we have fkPresent that describe present foreign keys
      -- and fkRequested that descript what is requested.
      --
      -- To sync these two we need to go through present list and drop
      -- everything that is not structurally equivalent to anything
      -- present on requested lists. Go through requested list and add
      -- anything that is not structurally equivalent to anything
      -- present.
      --
      -- Foreign key option change is modeled by drop and a following
      -- add. PostgreSQL is smart enough to optimize recalculation
      -- correctly.
      --
      -- We ignore the possibiliy of duplicate foreign keys that have
      -- same structure.
      --
      let foreignKeysToDrop = map fst (filter shouldDrop fkPresent)
          shouldDrop (_, fk) = not (any (structurallySame fk) fkRequested)
          foreignKeysToAdd = filter shouldAdd fkRequested
          shouldAdd fk = not (any (structurallySame fk) (map snd fkPresent))
          structurallySame fk1 fk2 = fk1 == fk2 -- for now it is good so

          actionToRawSQL ForeignKeyNoAction = "NO ACTION"
          actionToRawSQL ForeignKeyRestrict = "RESTRICT"
          actionToRawSQL ForeignKeyCascade = "CASCADE"
          actionToRawSQL ForeignKeySetNull = "SET NULL"
          actionToRawSQL ForeignKeySetDefault = "SET DEFAULT"

          foreignKeyName fk = "fk_" <> tblName table <> "_" <> mconcat (L.intersperse "_" (fkColumns fk)) <> "_" <> fkRefTable fk
          foreignKeysToAddNames = map foreignKeyName foreignKeysToAdd
          xdrop name = "DROP CONSTRAINT" <+> raw name
          xadd fk = "ADD FOREIGN KEY (" <> intersperse "," (map raw (fkColumns fk)) <> ")"
                <+> "REFERENCES" <+> raw (fkRefTable fk) <> "(" <> intersperse "," (map raw (fkRefColumns fk)) <> ")"
                <+> "ON UPDATE" <+> actionToRawSQL (fkOnUpdate fk)
                <+> "ON DELETE" <+> actionToRawSQL (fkOnDelete fk)
                <+> (if fkDeferrable fk then "DEFERRABLE" else "NOT DEFERRABLE")
                <+> (if fkDeferred fk then "INITIALLY DEFERRED" else "INITIALLY IMMEDIATE")

      forM_ (foreignKeysToDrop L.\\ foreignKeysToAddNames) $ \name -> do
        logger $ "   dropping foreign key '" ++ unRawSQL name ++ "'"
      forM_ (L.intersect foreignKeysToDrop foreignKeysToAddNames) $ \name -> do
        logger $ "   updating foreign key '" ++ unRawSQL name ++ "'"
      forM_ (foreignKeysToAddNames L.\\ foreignKeysToDrop) $ \name -> do
        logger $ "   adding foreign key '" ++ unRawSQL name ++ "'"

      when (not (null foreignKeysToDrop) || not (null foreignKeysToAdd)) $ do
         kRun_ $ "ALTER TABLE" <+> raw (tblName table)
                 <+> intersperse "," (map xdrop foreignKeysToDrop ++
                                     map xadd foreignKeysToAdd)

      return ()
