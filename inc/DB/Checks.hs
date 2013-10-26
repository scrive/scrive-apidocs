{-# LANGUAGE ExtendedDefaultRules #-}
module DB.Checks (
    performDBChecks
  ) where

import Control.Arrow (first, second)
import Control.Monad.Reader
import Data.Either
import Data.List.Utils
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Database.HDBC.SqlValue
import qualified Data.List as L
import qualified Data.Set as S

import DB.Core
import DB.Functions
import DB.Fetcher
import DB.Model
import DB.SQL
import DB.SQL2
import DB.Utils
import DB.Versions

default (SQL)

data ValidationResult =
    ValidationOk [SQL] -- ^ possible list of ALTER statements to execute
  | ValidationError String -- ^ error message

instance Monoid ValidationResult where
  mempty = ValidationOk []
  mappend (ValidationOk a) (ValidationOk b) = ValidationOk (mappend a b)
  mappend err@ValidationError{} _ = err
  mappend _ err@ValidationError{} = err

-- | Runs all checks on a database
performDBChecks :: MonadDB m => (String -> m ()) -> [Table] -> [Migration m] -> m ()
performDBChecks logger tables migrations = do
  set <- setByteaOutput logger
  when set $ do
    kCommit
    error $ "Bytea_output was changed to 'hex'. Restart application so the change is visible."
  checkDBTimeZone logger
  checkNeededExtensions logger
  checkDBConsistency logger (tableVersions : tables) migrations

  -- everything is OK, commit changes
  kCommit

  checkUnknownTables logger tables
  return ()

-- | Return SQL fragment of current catalog within quotes
currentCatalog :: MonadDB m => m RawSQL
currentCatalog = do
  Just dbname <- getOne "SELECT current_catalog"
  return $ unsafeFromString $ "\"" ++ dbname ++ "\""


checkNeededExtensions :: MonadDB m => (String -> m ()) -> m ()
checkNeededExtensions logger = do
  logger $ "Enabling needed extensions"
  kRunRaw $ "CREATE EXTENSION IF NOT EXISTS pgcrypto"
  return ()

-- |  Checks whether database returns timestamps in UTC
checkDBTimeZone :: MonadDB m => (String -> m ()) -> m ()
checkDBTimeZone logger = do
  dbname <- currentCatalog
  logger $ "Setting '" ++ unRawSQL dbname ++ "' database to return timestamps in UTC"
  kRunRaw $ "ALTER DATABASE " <> dbname <> " SET TIMEZONE = 'UTC'"
  return ()

setByteaOutput :: MonadDB m => (String -> m ()) -> m Bool
setByteaOutput logger = do
  dbname <- currentCatalog
  Just bytea_output <- getOne "SHOW bytea_output"
  if bytea_output /= ("hex" :: String)
    then do
      logger $ "Setting bytea_output to 'hex'..."
      kRunRaw $ "ALTER DATABASE " <> dbname <> " SET bytea_output = 'hex'"
      return True
    else return False


checkUnknownTables :: MonadDB m => (String -> m ()) -> [Table] -> m ()
checkUnknownTables logger tables = do
  kRun_ $ sqlSelect "information_schema.tables" $ do
        sqlResult "table_name"
        sqlWhere "table_name <> 'table_versions'"
        sqlWhere "table_type = 'BASE TABLE'"
        sqlWhere "table_schema NOT IN ('information_schema','pg_catalog')"
  desc <- kFold (\acc tn -> tn : acc) []
  let absent = desc L.\\ map (unRawSQL . tblName) tables
  when (not (null absent)) $
    mapM_ (\t -> logger $ "Unknown table '" ++ t ++ "': DROP TABLE " ++ t ++ " CASCADE") absent


-- | Checks whether database is consistent (performs migrations if necessary)
checkDBConsistency :: forall m. MonadDB m
                   => (String -> m ()) -> [Table] -> [Migration m]
                   -> m ()
checkDBConsistency logger tables migrations = do
  (created, to_migration) <- checkTables
  when (not $ null to_migration) $ do
    logger "Running migrations..."
    migrate migrations to_migration
    logger "Done."
    (_, to_migration_again) <- checkTables
    when (not $ null to_migration_again) $
      error $ "The following tables were not migrated to their latest versions: " ++ concatMap showNotMigrated to_migration_again
  forM_ created $ \table -> do
    logger $ arrListTable table ++ "putting properties..."
    -- if table was just created, create missing "outer" constraints
    checkIndexes table True
    checkForeignKeys table True
    tblPutProperties table
  forM_ tables $ \table -> do
    -- if table is old, check "outer" constraints for consistency (no autocreating)
    checkIndexes table False
    checkForeignKeys table False
  where
    tblNameString :: Table -> String
    tblNameString = unRawSQL . tblName

    showNotMigrated :: (Table, Int) -> String
    showNotMigrated (t, from) = "\n * " ++ tblNameString t ++ ", current version: " ++ show from ++ ", needed version: " ++ show (tblVersion t)

    checkTables :: m ([Table], [(Table, Int)])
    checkTables = (second catMaybes . partitionEithers) `liftM` mapM checkTable tables

    checkTable :: Table -> m (Either Table (Maybe (Table, Int)))
    checkTable table@Table{..} = do
      logger $ arrListTable table ++ "checking version..."
      mver <- checkVersion table
      case mver of
        Nothing -> do
          let sql = createStatement table
          logger $ "Executing " ++ show sql ++ "..."
          kRun_ sql
          kRun_ . sqlInsert "table_versions" $ do
            sqlSet "name" (tblNameString table)
            sqlSet "version" tblVersion
          _ <- checkTable table
          return $ Left table
        Just ver | ver == tblVersion -> do
          logger $ arrListTable table ++ "checking structure (v" ++ show ver ++ ")..."
          validation <- checkTableStructure table
          case validation of
            ValidationError errmsg -> do
              logger errmsg
              error $ "Existing '" ++ tblNameString table ++ "' table structure is invalid."
            ValidationOk sqls -> do
              forM_ sqls $ \sql -> do
                logger $ "Executing " ++ show sql ++ "..."
                kRun_ sql
              return $ Right Nothing
        Just ver | ver < tblVersion -> do
          logger $ arrListTable table ++ "scheduling for migration " ++ show ver ++ " => " ++ show tblVersion
          return . Right . Just $ (table, ver)
        Just ver -> error $ "Table '" ++ tblNameString table ++ "' in the database has higher version than the definition (database: " ++ show ver ++ ", definition: " ++ show tblVersion ++ ")"

    checkVersion :: Table -> m (Maybe Int)
    checkVersion table = do
      doesExist <- getOne $ sqlSelect "pg_catalog.pg_class c" $ do
        sqlResult "TRUE"
        sqlLeftJoinOn "pg_catalog.pg_namespace n" "n.oid = c.relnamespace"
        sqlWhereEq "c.relname" $ tblNameString table
        sqlWhere "pg_catalog.pg_table_is_visible(c.oid)"
      case doesExist of
        Just (_::Bool) -> do
          mver <- getOne $ SQL "SELECT version FROM table_versions WHERE name = ?" [toSql $ tblNameString table]
          case mver of
            Just ver -> return $ Just ver
            Nothing  -> error $ "Table '" ++ tblNameString table ++ "' is present in the database, but there is no corresponding version info in 'table_versions'."
        Nothing -> do
          -- if table is not present, it will be created and version info attempted
          -- to be written to table_versions, so if it's already there, delete it
          deleted <- kRun01 $ SQL "DELETE FROM table_versions WHERE name = ?" [toSql $ tblNameString table]
          when deleted $ do
            logger $ "Old version info of table '" ++ tblNameString table ++ "' deleted from 'table_versions'."
          return Nothing

    migrate :: [Migration m] -> [(Table, Int)] -> m ()
    migrate ms ts = forM_ ms $ \m -> forM_ ts $ \(t, from) -> do
      if tblName (mgrTable m) == tblName t && mgrFrom m >= from
         then do
           Just ver <- checkVersion $ mgrTable m
           logger $ arrListTable tblTable ++ "migrating: " ++ show ver ++ " => " ++ show (mgrFrom m + 1) ++ "..."
           when (ver /= mgrFrom m) $
             error $ "Migrations are in wrong order in migrations list."
           mgrDo m
           kRun_ $ SQL "UPDATE table_versions SET version = ? WHERE name = ?"
                   [toSql $ succ $ mgrFrom m, toSql $ tblNameString t]
           return ()
         else return ()

    createStatement :: Table -> SQL
    createStatement table@Table{..} = mconcat [
        raw $ "CREATE TABLE" <+> tblName <+> "("
      , intersperseNoWhitespace ", " $ map columnToSQL tblColumns
      , if primaryKeyToSQL table == mempty then "" else ", "
      , primaryKeyToSQL table
      , if uniquesToSQL == mempty then "" else ", "
      , uniquesToSQL
      , if checksToSQL == mempty then "" else ", "
      , checksToSQL
      , ")"
      ]
      where
        uniquesToSQL = intersperseNoWhitespace ", " $ map (uniqueToSQL table) tblUniques
        checksToSQL = intersperseNoWhitespace ", " $ map (checkToSQL table) tblChecks

    colTypeToSQL :: ColumnType -> SQL
    colTypeToSQL BigIntT = "BIGINT"
    colTypeToSQL BigSerialT = "BIGSERIAL"
    colTypeToSQL BinaryT = "BYTEA"
    colTypeToSQL BoolT = "BOOLEAN"
    colTypeToSQL DateT = "DATE"
    colTypeToSQL DoubleT = "DOUBLE PRECISION"
    colTypeToSQL IntegerT = "INTEGER"
    colTypeToSQL SmallIntT = "SMALLINT"
    colTypeToSQL TextT = "TEXT"
    colTypeToSQL TimestampWithZoneT = "TIMESTAMPTZ"

    colTypeToSQLForAlter :: ColumnType -> SQL
    colTypeToSQLForAlter BigSerialT = colTypeToSQL BigIntT
    colTypeToSQLForAlter x = colTypeToSQL x

    primaryKeyToSQL :: Table -> SQL
    primaryKeyToSQL table@Table{..} = if null tblPrimaryKey
      then ""
      else "CONSTRAINT"
        <+> genPrimaryKeyName table
        <+> "PRIMARY KEY ("
        <+> intersperseNoWhitespace ", " (map raw tblPrimaryKey)
        <+> ")"

    uniqueToSQL :: Table -> [RawSQL] -> SQL
    uniqueToSQL table unique = "CONSTRAINT"
      <+> genUniqueName table unique
      <+> "UNIQUE ("
      <+> intersperseNoWhitespace ", " (map raw unique)
      <+> ")"

    checkToSQL :: Table -> TableCheck -> SQL
    checkToSQL table@Table{..} chk@TableCheck{..} = "CONSTRAINT"
      <+> genCheckName table chk
      <+> "CHECK ("
      <+> raw chkCondition
      <+> ")"

    genPrimaryKeyName :: Table -> SQL
    genPrimaryKeyName Table{..} = mconcat ["pk__", raw tblName]

    genUniqueName :: Table -> [RawSQL] -> SQL
    genUniqueName Table{..} cols = mconcat [
        "unique__"
      , raw tblName
      , "__"
      , intersperseNoWhitespace "_" (map raw cols)
      ]

    genCheckName :: Table -> TableCheck -> SQL
    genCheckName Table{..} TableCheck{chkName} = mconcat [
        "check__"
      , raw tblName
      , "__"
      , raw chkName
      ]

    columnToSQL :: TableColumn -> SQL
    columnToSQL TableColumn{..} = raw colName
      <+> colTypeToSQL colType
      <+> (if colNullable then "NULL" else "NOT NULL")
      <+> raw (maybe "" ("DEFAULT" <+>) colDefault)

    sqlGetTableID :: Table -> SQL
    sqlGetTableID table = parenthesize . toSQLCommand $
      sqlSelect "pg_catalog.pg_class c" $ do
        sqlResult "c.oid"
        sqlLeftJoinOn "pg_catalog.pg_namespace n" "n.oid = c.relnamespace"
        sqlWhereEq "c.relname" $ tblNameString table
        sqlWhere "pg_catalog.pg_table_is_visible(c.oid)"

    checkTableStructure :: Table -> m ValidationResult
    checkTableStructure table@Table{..} = do
      -- get table description from pg_catalog as describeTable
      -- mechanism from HDBC doesn't give accurate results
      kRun_ $ sqlSelect "pg_catalog.pg_attribute a" $ do
        sqlResult "a.attname"
        sqlResult "pg_catalog.format_type(a.atttypid, a.atttypmod)"
        sqlResult "NOT a.attnotnull"
        sqlResult . parenthesize . toSQLCommand $
          sqlSelect "pg_catalog.pg_attrdef d" $ do
            sqlResult "pg_catalog.pg_get_expr(d.adbin, d.adrelid)"
            sqlWhere "d.adrelid = a.attrelid"
            sqlWhere "d.adnum = a.attnum"
            sqlWhere "a.atthasdef"
        sqlWhere "a.attnum > 0"
        sqlWhere "NOT a.attisdropped"
        sqlWhereEqSql "a.attrelid" $ sqlGetTableID table
        sqlOrderBy "a.attnum"
      desc <- reverse `liftM` kFold fetchTableColumn []
      -- get info about constraints from pg_catalog
      kRun_ $ sqlGetConstraintOfType 'p' -- primary key
      pk <- kFold fetchTablePrimaryKey Nothing
      kRun_ $ sqlGetConstraintOfType 'u' -- uniques
      uniques <- kFold fetchTableUniques []
      kRun_ $ sqlGetConstraintOfType 'c' -- checks
      checks <- kFold fetchTableChecks []
      return $ mconcat [
          checkColumns 0 tblColumns desc
        , checkPrimaryKey tblPrimaryKey pk
        , checkUniques tblUniques uniques
        , checkChecks tblChecks checks
        ]
      where
        fetchTableColumn :: [TableColumn] -> String -> ColumnType -> Bool -> Maybe String -> [TableColumn]
        fetchTableColumn acc name ctype nullable mdefault = TableColumn {
            colName = unsafeFromString name
          , colType = ctype
          , colNullable = nullable
          , colDefault = unsafeFromString `liftM` mdefault
          } : acc

        fetchTablePrimaryKey :: Maybe (RawSQL, Set RawSQL) -> String -> String -> Maybe (RawSQL, Set RawSQL)
        fetchTablePrimaryKey Nothing name columns =
          Just (unsafeFromString name, S.fromList $ map unsafeFromString $ split "," columns)
        fetchTablePrimaryKey _ _ _ =
          error $ "fetchTablePrimaryKey (" ++ tblNameString table ++ "): more than one primary key (shouldn't happen)"

        fetchTableUniques :: [(Set RawSQL, RawSQL)] -> String -> String -> [(Set RawSQL, RawSQL)]
        fetchTableUniques acc name columns = (S.fromList $ map unsafeFromString $ split "," columns, unsafeFromString name) : acc

        fetchTableChecks :: [(RawSQL, RawSQL)] -> String -> String -> [(RawSQL, RawSQL)]
        fetchTableChecks acc name condition = (unsafeFromString condition, unsafeFromString name) : acc

        sqlGetConstraintOfType :: Char -> SQL
        sqlGetConstraintOfType ctype = toSQLCommand $
          sqlSelect "pg_catalog.pg_constraint c" $ do
            sqlResult "c.conname"
            sqlResult $ if ctype == 'c'
              then "regexp_replace(pg_get_constraintdef(c.oid, true), 'CHECK \\((.*)\\)', '\\1')" -- check body
              else "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = c.conrelid AND a.attnum = ANY (c.conkey)), ',')" -- list of affected columns (unique, primary key)
            sqlWhereEq "c.contype" ctype
            sqlWhereEqSql "c.conrelid" $ sqlGetTableID table

        checkColumns :: Int -> [TableColumn] -> [TableColumn] -> ValidationResult
        checkColumns _ [] [] = mempty
        checkColumns _ rest [] = ValidationError $ "Table '" ++ tblNameString table ++ "' needs to have columns added: " ++ show (map (\col -> "ALTER TABLE" <+> raw tblName <+> "ADD COLUMN" <+> columnToSQL col) rest)
        checkColumns !n [] rest = ValidationError $ "Table '" ++ tblNameString table ++ "' in database has too many columns (definition: " ++ show n ++ ", database: " ++ show (n + length rest) ++ ")"
        checkColumns !n (d:defs) (c:cols) = mconcat [
            validateNames $ colName d == colName c
          -- bigserial == bigint + autoincrement and there is no
          -- distinction between them after table is created.
          , validateTypes $ colType d == colType c || (colType d == BigSerialT && colType c == BigIntT)
          -- there is a problem with default values determined by sequences as
          -- they're implicitely specified by db, so let's omit them in such case
          , validateDefaults $ colDefault d == colDefault c || (colDefault d == Nothing && ((L.isPrefixOf "nextval('" . unRawSQL) `liftM` colDefault c) == Just True)
          , validateNullables $ colNullable d == colNullable c
          , checkColumns (n+1) defs cols
          ]
          where
            validateNames True = mempty
            validateNames False = ValidationError $ errorMsg ("no. " ++ show (n+1)) "names" (unRawSQL . colName)
            validateTypes True = mempty
            validateTypes False = ValidationOk ["ALTER TABLE" <+> raw tblName <+> "ALTER COLUMN" <+> raw (colName d) <+> "TYPE" <+> colTypeToSQLForAlter (colType d)]
            validateNullables True = mempty
            validateNullables False = ValidationOk ["ALTER TABLE" <+> raw tblName <+> "ALTER COLUMN" <+> raw (colName d) <+> (if colNullable d then "DROP" else "SET") <+> "NOT NULL"]
            validateDefaults True = mempty
            validateDefaults False = ValidationOk ["ALTER TABLE" <+> raw tblName <+> "ALTER COLUMN" <+> raw (colName d) <+> set_default]
              where
                set_default = case colDefault d of
                  Just v -> "SET DEFAULT" <+> raw v
                  Nothing -> "DROP DEFAULT"

            errorMsg ident attr f = "Column " ++ ident ++ " differs in " ++ attr ++ " (definition: " ++ f d ++ ", database: " ++ f c ++ ")"

        checkPrimaryKey :: [RawSQL] -> Maybe (RawSQL, Set RawSQL) -> ValidationResult
        checkPrimaryKey [] Nothing = mempty
        checkPrimaryKey _ Nothing = ValidationOk ["ALTER TABLE " <+> raw tblName <+> "ADD" <+> primaryKeyToSQL table]
        checkPrimaryKey [] (Just (name, _cols)) = ValidationOk ["ALTER TABLE " <+> raw tblName <+> "DROP CONSTRAINT" <+> raw name]
        checkPrimaryKey def (Just (name, cols))
          | S.fromList def == cols = ValidationOk $ if raw name /= genPrimaryKeyName table
            -- rename index that represents primary key
            then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> genPrimaryKeyName table]
            else []
          | otherwise = ValidationError $ "Primary key in table definition (" ++ L.intercalate ", " (map unRawSQL def) ++ ") differs from the one in the database (" ++ L.intercalate ", " (map unRawSQL $ S.toList cols) ++ ")"

        checkUniques :: [[RawSQL]] -> [(Set RawSQL, RawSQL)] -> ValidationResult
        checkUniques [] [] = mempty
        checkUniques rest [] = ValidationOk $ map (\unique -> "ALTER TABLE" <+> raw tblName <+> "ADD CONTRAINT" <+> uniqueToSQL table unique) rest
        checkUniques [] rest = ValidationError $ "Table in database has more UNIQUE constraints than definition (" ++ showProperties rest ++ ")"
        checkUniques (d:defs) uniques = mconcat [
            case sd `L.lookup` uniques of
              Nothing -> mempty
              Just name -> ValidationOk $ if raw name /= genUniqueName table d
                then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> genUniqueName table d]
                else []
          , checkUniques defs new_uniques
          ]
          where
            sd = S.fromList d
            new_uniques = deleteFirst ((== sd) . fst) uniques

        checkChecks :: [TableCheck] -> [(RawSQL, RawSQL)] -> ValidationResult
        checkChecks [] [] = mempty
        checkChecks rest [] = ValidationOk $ map (\chk -> "ALTER TABLE" <+> raw tblName <+> "ADD" <+> checkToSQL table chk) rest
        checkChecks [] rest = ValidationError $ "Table in database has more CHECK constraints that definition (" ++ showProperties (map (first S.singleton) rest) ++ ")"
        checkChecks (d:defs) checks = mconcat [
            case chkCondition d `L.lookup` checks of
              Just name -> ValidationOk $ if genCheckName table d /= raw name
                -- remove old check and add new, there is no way to rename it
                then ["ALTER TABLE" <+> raw tblName <+> "DROP CONSTRAINT" <+> raw name <+> "," <+> "ADD" <+> checkToSQL table d]
                else []
              -- it may happen that the body is different, but there already is
              -- a check with that name. so let's check if there exist a check
              -- with the same name, but different condition and display
              -- appropriate error message if that's the case.
              Nothing -> case filter ((== genCheckName table d) . raw . snd) checks of
                []  -> mempty
                [(condition, name)] -> ValidationError $ "Check " ++ unRawSQL name ++ " in database has different condition [" ++ unRawSQL condition ++ "] than the one in the definition [" ++ unRawSQL (chkCondition d) ++ "]"
                _ -> error "checkChecks: more that one CHECK with the same name in the definition (shouldn't happen)"
          , checkChecks defs new_checks
          ]
          where
            new_checks = deleteFirst ((== chkCondition d) . fst) checks

    checkIndexes :: Table -> Bool -> m ()
    checkIndexes table@Table{..} create_missing = do
      logger $ arrListTable table ++ "checking indexes..."
      kRun_ . sqlSelect "pg_catalog.pg_class c" $ do
        sqlResult "c.relname" -- index name
        sqlResult "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = i.indexrelid), ',')" -- array of affected columns
        sqlJoinOn "pg_catalog.pg_index i" "c.oid = i.indexrelid"
        sqlLeftJoinOn "pg_catalog.pg_constraint r" "r.conindid = i.indexrelid"
        sqlWhereEqSql "i.indrelid" $ sqlGetTableID table
        sqlWhereIsNULL "r.contype" -- omit primary keys and column uniques
      indexes <- kFold fetchTableIndexes []
      -- create indexes on columns marked as foreign keys to speed things up
      let fk_indexes = map (tblIndexOnColumns . fkColumns) tblForeignKeys
          indexes_definition = L.nub (tblIndexes ++ fk_indexes)
      case validate indexes_definition indexes of
        ValidationError errmsg -> do
          logger errmsg
          error $ "Indexes on table '" ++ tblNameString table ++ "' are invalid."
        ValidationOk sqls -> forM_ sqls $ \sql -> do
          logger $ "Executing " ++ show sql ++ "..."
          kRun_ sql
      where
        genIndexName :: TableIndex -> SQL
        genIndexName TableIndex{..} = mconcat [
            "idx__"
          , raw tblName
          , "__"
          , intersperseNoWhitespace "__" (map raw tblIndexColumns)
          ]

        indexToSQL :: TableIndex -> SQL
        indexToSQL idx@TableIndex{..} = mconcat [
            "CREATE INDEX" <+> genIndexName idx <+> "ON" <+> raw tblName <+> "("
          , intersperseNoWhitespace ", " (map raw tblIndexColumns)
          , ")"
          ]

        fetchTableIndexes :: [(Set RawSQL, RawSQL)] -> String -> String -> [(Set RawSQL, RawSQL)]
        fetchTableIndexes acc name columns = (S.fromList . map unsafeFromString . split "," $ columns, unsafeFromString name) : acc

        validate :: [TableIndex] -> [(Set RawSQL, RawSQL)] -> ValidationResult
        validate [] [] = mempty
        validate rest [] = if create_missing
         then ValidationOk $ map indexToSQL rest
         else ValidationError $ "Table in database has less indexes than definition (missing: " ++ show rest ++ ")"
        validate [] rest = ValidationError $ "Table in database has more indexes than definition (" ++ showProperties rest ++ ")"
        validate (d:defs) indexes = mconcat [
            case sd `L.lookup` indexes of
              Nothing -> mempty
              Just name -> ValidationOk $ if raw name /= genIndexName d
                then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> genIndexName d]
                else []
          , validate defs new_indexes
          ]
          where
            sd = S.fromList . tblIndexColumns $ d
            new_indexes = deleteFirst ((== sd) . fst) indexes

    checkForeignKeys :: Table -> Bool -> m ()
    checkForeignKeys table@Table{..} create_missing = do
      logger $ arrListTable table ++ "checking foreign keys..."
      kRun_ . sqlSelect "pg_catalog.pg_constraint r" $ do
        sqlResult "r.conname" -- fk name
        sqlResult "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = r.conrelid AND a.attnum = ANY (r.conkey)), ',') as columns" -- constrained columns
        sqlResult "c.relname" -- referenced table
        sqlResult "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = r.confrelid AND a.attnum = ANY (r.confkey)), ',') as refcolumns" -- referenced columns
        sqlResult "r.confupdtype" -- on update
        sqlResult "r.confdeltype" -- on delete
        sqlResult "r.condeferrable" -- deferrable?
        sqlResult "r.condeferred" -- initially deferred?
        sqlJoinOn "pg_catalog.pg_class c" "c.oid = r.confrelid"
        sqlWhereEqSql "r.conrelid" $ sqlGetTableID table
        sqlWhereEq "r.contype" 'f'
      fks <- kFold fetchForeignKeys []
      case validate tblForeignKeys fks of
        ValidationError errmsg -> do
          logger errmsg
          error $ "Foreign keys on table '" ++ tblNameString table ++ "' are invalid."
        ValidationOk sqls -> forM_ sqls $ \sql -> do
          logger $ "Executing " ++ show sql ++ "..."
          kRun_ sql
      where
        genForeignKeyName :: ForeignKey -> SQL
        genForeignKeyName ForeignKey{..} = mconcat [
            "fk__"
          , raw tblName
          , "__"
          , intersperseNoWhitespace "_" (map raw fkColumns)
          , "__"
          , raw fkRefTable
          ]

        foreignKeyToSQL :: ForeignKey -> SQL
        foreignKeyToSQL fk@ForeignKey{..} = mconcat [
            "ADD CONSTRAINT" <+> genForeignKeyName fk <+> "FOREIGN KEY ("
          , intersperseNoWhitespace ", " (map raw fkColumns)
          , ") REFERENCES" <+> raw fkRefTable <+> "("
          , intersperseNoWhitespace ", " (map raw fkRefColumns)
          , ") ON UPDATE" <+> foreignKeyActionToSQL fkOnUpdate
          , "  ON DELETE" <+> foreignKeyActionToSQL fkOnDelete
          , " " <> if fkDeferrable then "DEFERRABLE" else "NOT DEFERRABLE"
          , " INITIALLY" <+> if fkDeferred then "DEFERRED" else "IMMEDIATE"
          ]
          where
            foreignKeyActionToSQL ForeignKeyNoAction = "NO ACTION"
            foreignKeyActionToSQL ForeignKeyRestrict = "RESTRICT"
            foreignKeyActionToSQL ForeignKeyCascade = "CASCADE"
            foreignKeyActionToSQL ForeignKeySetNull = "SET NULL"
            foreignKeyActionToSQL ForeignKeySetDefault = "SET DEFAULT"

        fetchForeignKeys :: [(ForeignKey, RawSQL)] -> String -> String -> String -> String -> Char -> Char -> Bool -> Bool -> [(ForeignKey, RawSQL)]
        fetchForeignKeys acc name columns reftable refcolumns on_update on_delete deferrable deferred = (ForeignKey {
            fkColumns = map unsafeFromString . split "," $ columns
          , fkRefTable = unsafeFromString reftable
          , fkRefColumns = map unsafeFromString . split "," $ refcolumns
          , fkOnUpdate = charToForeignKeyAction on_update
          , fkOnDelete = charToForeignKeyAction on_delete
          , fkDeferrable = deferrable
          , fkDeferred = deferred
          }, unsafeFromString name) : acc
          where
            charToForeignKeyAction c = case c of
              'a' -> ForeignKeyNoAction
              'r' -> ForeignKeyRestrict
              'c' -> ForeignKeyCascade
              'n' -> ForeignKeySetNull
              'd' -> ForeignKeySetDefault
              _   -> error $ "Invalid foreign key action code: " ++ show c

        validate :: [ForeignKey] -> [(ForeignKey, RawSQL)] -> ValidationResult
        validate [] [] = mempty
        validate rest [] = if create_missing
         then ValidationOk $ ["ALTER TABLE" <+> raw tblName <+> intersperseNoWhitespace ", " (map foreignKeyToSQL rest)]
         else ValidationError $ "Table in database has less foreign keys than definition (missing: " ++ show rest ++ ")"
        validate [] rest = ValidationError $ "Table in database has more foreign keys than definition (" ++ show rest ++ ")"
        validate (d:defs) fkeys = mconcat [
            case d `L.lookup` fkeys of
              Nothing -> mempty
              Just name -> ValidationOk $ if raw name /= genForeignKeyName d
                then ["ALTER TABLE" <+> raw tblName <+> "DROP CONSTRAINT" <+> raw name <+> "," <+> foreignKeyToSQL d]
                else []
          , validate defs new_fkeys
          ]
          where
            new_fkeys = deleteFirst ((== d) . fst) fkeys

    arrListTable :: Table -> String
    arrListTable table = " -> " ++ tblNameString table ++ ": "

    showProperties :: [(Set RawSQL, RawSQL)] -> String
    showProperties = L.intercalate ", " . map (\(columns, name) -> concat [
        unRawSQL name
      , "["
      , L.intercalate ", " . map unRawSQL . S.toList $ columns
      , "]"
      ])

    deleteFirst :: (a -> Bool) -> [a] -> [a]
    deleteFirst f xs = let (a, b) = break f xs in a ++ drop 1 b
