{-# LANGUAGE ExtendedDefaultRules #-}
module DB.Checks (
    performDBChecks
  ) where

import Control.Arrow (second)
import Control.Monad.Reader
import Data.Either
import Data.List.Utils
import Data.Maybe
import Data.Monoid
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
  -- run migrations, if necessary
  when (not . null $ to_migration) $ do
    logger "Running migrations..."
    migrate migrations to_migration
    logger "Done."
    (_, to_migration_again) <- checkTables
    when (not . null $ to_migration_again) $
      error $ "The following tables were not migrated to their latest versions: " ++ concatMap showNotMigrated to_migration_again
  -- add foreign keys and properties to new tables
  forM_ created $ \table@Table{..} -> do
    when (not . null $ tblForeignKeys) $ do
      logger $ arrListTable table ++ "creating foreign keys..."
      logged kRun_ $ sqlAlterTable tblName $ map (sqlAddFK tblName) tblForeignKeys
    logger $ arrListTable table ++ "putting properties..."
    tblPutProperties
  -- check foreign key consistency
  forM_ tables checkForeignKeys
  where
    logged :: (SQL -> m a) -> SQL -> m a
    logged exec sql = do
      logger $ "Executing " ++ show sql ++ "..."
      exec sql

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
          forM_ createStatements $ logged kRun_
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
              forM_ sqls $ logged kRun_
              return $ Right Nothing
        Just ver | ver < tblVersion -> do
          logger $ arrListTable table ++ "scheduling for migration " ++ show ver ++ " => " ++ show tblVersion
          return . Right . Just $ (table, ver)
        Just ver -> error $ "Table '" ++ tblNameString table ++ "' in the database has higher version than the definition (database: " ++ show ver ++ ", definition: " ++ show tblVersion ++ ")"
      where
        createStatements = concat [
            [sqlCreateTable tblName]
          , if null tblColumns
              then []
              else [sqlAlterTable tblName $ map sqlAddColumn tblColumns]
          , case tblPrimaryKey of
              Nothing -> []
              Just pk -> [sqlAlterTable tblName [sqlAddPK tblName pk]]
          , if null tblChecks
            then []
            else [sqlAlterTable tblName $ map (sqlAddCheck tblName) tblChecks]
          , map (sqlCreateIndex tblName) tblIndexes
          ]

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
          when (tblName table /= tblName tableVersions) $ do
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

    -- *** TABLE STRUCTURE ***

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
      pk <- kFold fetchPrimaryKey Nothing
      kRun_ $ sqlGetConstraintOfType 'c' -- checks
      checks <- kFold fetchTableChecks []
      kRun_ $ sqlGetIndexes table
      indexes <- kFold fetchTableIndexes []
      return $ mconcat [
          checkColumns 1 tblColumns desc
        , checkPrimaryKey tblPrimaryKey pk
        , checkChecks tblChecks checks
        , checkIndexes tblIndexes indexes
        ]
      where
        fetchTableColumn :: [TableColumn] -> String -> ColumnType -> Bool -> Maybe String -> [TableColumn]
        fetchTableColumn acc name ctype nullable mdefault = TableColumn {
            colName = unsafeFromString name
          , colType = ctype
          , colNullable = nullable
          , colDefault = unsafeFromString `liftM` mdefault
          } : acc

        fetchPrimaryKey :: Maybe (RawSQL, PrimaryKey) -> String -> String -> Maybe (RawSQL, PrimaryKey)
        fetchPrimaryKey Nothing name columns =
          Just (unsafeFromString name, unsafePrimaryKey . S.fromList . map unsafeFromString . split "," $ columns)
        fetchPrimaryKey _ _ _ =
          error $ "fetchPrimaryKey (" ++ tblNameString table ++ "): more than one primary key (shouldn't happen)"

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
        checkColumns _ rest [] = ValidationError $ databaseHasLess "columns" rest
        checkColumns _ [] rest = ValidationError $ databaseHasMore "columns" rest
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
            validateNames False = ValidationError $ errorMsg ("no. " ++ show n) "names" (unRawSQL . colName)

            validateTypes True = mempty
            validateTypes False = ValidationError $ errorMsg cname "types" (show . colType)

            validateNullables True = mempty
            validateNullables False = ValidationError $ errorMsg cname "nullables" (show . colNullable)

            validateDefaults True = mempty
            validateDefaults False = ValidationError $ errorMsg cname "defaults" (show . colDefault)

            cname = unRawSQL . colName $ d
            errorMsg ident attr f = "Column " ++ ident ++ " differs in " ++ attr ++ " (definition: " ++ f d ++ ", database: " ++ f c ++ ")"

        checkPrimaryKey :: Maybe PrimaryKey -> Maybe (RawSQL, PrimaryKey) -> ValidationResult
        checkPrimaryKey Nothing Nothing = mempty
        checkPrimaryKey pk Nothing = ValidationError $ databaseHasLess "PRIMARY KEYs" pk
        checkPrimaryKey Nothing (Just (_, pk)) = ValidationError $ databaseHasMore "PRIMARY KEYs" pk
        checkPrimaryKey (Just def) (Just (name, pk))
          | def == pk = ValidationOk $ if raw name /= pkName tblName
            -- rename index that represents primary key
            then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> pkName tblName]
            else []
          | otherwise = ValidationError $ "Primary key in table definition (" ++ show def ++ ") differs from the one in the database (" ++ show pk ++ ")"

        checkChecks :: [TableCheck] -> [(RawSQL, RawSQL)] -> ValidationResult
        checkChecks [] [] = mempty
        checkChecks rest [] = ValidationError $ databaseHasLess "CHECKs" rest
        checkChecks [] rest = ValidationError $ databaseHasMore "CHECKs" rest
        checkChecks (d:defs) checks = mconcat [
            case chkCondition d `L.lookup` checks of
              Just name -> ValidationOk $ if checkName tblName d /= raw name
                -- remove old check and add new, there is no way to rename it
                then ["ALTER TABLE" <+> raw tblName <+> "DROP CONSTRAINT" <+> raw name <+> "," <+> sqlAddCheck tblName d]
                else []
              -- it may happen that the body is different, but there already is
              -- a check with that name. so let's check if there exist a check
              -- with the same name, but different condition and display
              -- appropriate error message if that's the case.
              Nothing -> case filter ((== checkName tblName d) . raw . snd) checks of
                []  -> mempty
                [(condition, name)] -> ValidationError $ "Check " ++ unRawSQL name ++ " in the database has different condition [" ++ unRawSQL condition ++ "] than the one in the definition [" ++ unRawSQL (chkCondition d) ++ "] (HINT: If the bodies are equal modulo number of parentheses, just copy and paste suggested body into source code)"
                _ -> error "checkChecks: more that one CHECK with the same name in the definition (shouldn't happen)"
          , checkChecks defs $ deleteFirst ((== chkCondition d) . fst) checks
          ]

        checkIndexes :: [TableIndex] -> [(TableIndex, RawSQL)] -> ValidationResult
        checkIndexes [] [] = mempty
        checkIndexes rest [] = ValidationError $ databaseHasLess "INDEXes" rest
        checkIndexes [] rest = ValidationError $ databaseHasMore "INDEXes" rest
        checkIndexes (d:defs) indexes = mconcat [
            case d `L.lookup` indexes of
              Nothing -> mempty
              Just name -> ValidationOk $ if raw name /= indexName tblName d
                then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> indexName tblName d]
                else []
          , checkIndexes defs $ deleteFirst ((== d) . fst) indexes
          ]

    -- *** INDEXES ***

    sqlGetIndexes :: Table -> SQL
    sqlGetIndexes table = toSQLCommand . sqlSelect "pg_catalog.pg_class c" $ do
      sqlResult "c.relname" -- index name
      sqlResult "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = i.indexrelid), ',')" -- array of affected columns
      sqlResult "i.indisunique" -- is it unique?
      sqlJoinOn "pg_catalog.pg_index i" "c.oid = i.indexrelid"
      sqlLeftJoinOn "pg_catalog.pg_constraint r" "r.conindid = i.indexrelid"
      sqlWhereEqSql "i.indrelid" $ sqlGetTableID table
      sqlWhere "(r.contype IS NULL OR r.contype = 'u')" -- omit primary key

    fetchTableIndexes :: [(TableIndex, RawSQL)] -> String -> String -> Bool -> [(TableIndex, RawSQL)]
    fetchTableIndexes acc name columns unique = (TableIndex {
        idxColumns = S.fromList . map unsafeFromString . split "," $ columns
      , idxUnique = unique
      }, unsafeFromString name) : acc

    -- *** FOREIGN KEYS ***

    sqlGetForeignKeys :: Table -> SQL
    sqlGetForeignKeys table = toSQLCommand . sqlSelect "pg_catalog.pg_constraint r" $ do
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

    fetchForeignKeys :: [(ForeignKey, RawSQL)] -> String -> String -> String -> String -> Char -> Char -> Bool -> Bool -> [(ForeignKey, RawSQL)]
    fetchForeignKeys acc name columns reftable refcolumns on_update on_delete deferrable deferred = (ForeignKey {
        fkColumns = S.fromList . map unsafeFromString . split "," $ columns
      , fkRefTable = unsafeFromString reftable
      , fkRefColumns = S.fromList . map unsafeFromString . split "," $ refcolumns
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

    checkForeignKeys :: Table -> m ()
    checkForeignKeys table@Table{..} = do
      logger $ arrListTable table ++ "checking foreign keys..."
      kRun_ $ sqlGetForeignKeys table
      fks <- kFold fetchForeignKeys []
      case validate tblForeignKeys fks of
        ValidationError errmsg -> do
          logger errmsg
          error $ "Foreign keys on table '" ++ tblNameString table ++ "' are invalid."
        ValidationOk sqls -> forM_ sqls $ logged kRun_
      where
        validate :: [ForeignKey] -> [(ForeignKey, RawSQL)] -> ValidationResult
        validate [] [] = mempty
        validate rest [] = ValidationError $ databaseHasLess "FOREIGN KEYs" rest
        validate [] rest = ValidationError $ databaseHasMore "FOREIGN KEYs" rest
        validate (d:defs) fkeys = mconcat [
            case d `L.lookup` fkeys of
              Nothing -> mempty
              Just name -> ValidationOk $ if raw name /= fkName tblName d
                then ["ALTER TABLE" <+> raw tblName <+> "DROP CONSTRAINT" <+> raw name <+> "," <+> sqlAddFK tblName d]
                else []
          , validate defs $ deleteFirst ((== d) . fst) fkeys
          ]

    -- *** UTILS ***

    databaseHasLess :: Show t => String -> t -> String
    databaseHasLess ptype missing = "Table in the database has *less* " ++ ptype ++ " than the definition (missing: " ++ show missing ++ ")"

    databaseHasMore :: Show t => String -> t -> String
    databaseHasMore ptype extra = "Table in the database has *more* " ++ ptype ++ " than the definition (extra: " ++ show extra ++ ")"

    arrListTable :: Table -> String
    arrListTable table = " -> " ++ tblNameString table ++ ": "

    deleteFirst :: (a -> Bool) -> [a] -> [a]
    deleteFirst f xs = let (a, b) = break f xs in a ++ drop 1 b
