{-# LANGUAGE ExtendedDefaultRules #-}
module DB.Checks (
    migrateDatabase
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

newtype ValidationResult = ValidationResult [String] -- ^ list of error messages

instance Monoid ValidationResult where
  mempty = ValidationResult []
  mappend (ValidationResult a) (ValidationResult b) = ValidationResult (a ++ b)

-- | Runs all checks on a database
migrateDatabase :: MonadDB m => (String -> m ()) -> [Table] -> [Migration m] -> m ()
migrateDatabase logger tables migrations = do
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

  versions <- mapM checkTableVersion tables

  let to_create = catMaybes (zipWith (\table ver -> if isNothing ver then Just table else Nothing) tables versions)
  let to_migration = catMaybes (zipWith (\table ver -> case ver of
                                                          Just v -> Just (table,v)
                                                          _ -> Nothing) tables versions)

  when (not . null $ to_create) $
    forM_ to_create $ \table@Table{..} -> do
      forM_ (createTableSQLs table) $ logged kRun_
      -- Database might have a stale version entry in
      -- 'table_versions'. Remove it here.
      kRun_ .sqlDelete "table_versions" $ do
        sqlWhereEq "name" (tblNameString table)
      kRun_ . sqlInsert "table_versions" $ do
        sqlSet "name" (tblNameString table)
        sqlSet "version" tblVersion
      result <- checkTableStructure table
      case result of
        ValidationResult [] -> return ()
        ValidationResult errmsgs -> do
          mapM_ logger errmsgs
          error "Failed"

  mapM_ normalizePropertyNames tables

  -- run migrations, if necessary
  when (not . null $ to_migration) $ do
    logger "Running migrations..."
    migrate migrations to_migration
    logger "Done."
    (_, to_migration_again) <- checkTables
    when (not . null $ to_migration_again) $
      error $ "The following tables were not migrated to their latest versions: " ++ concatMap showNotMigrated to_migration_again

  -- add foreign keys and properties to new tables
  forM_ to_create $ \table@Table{..} -> do
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

    createTableSQLs :: Table -> [SQL]
    createTableSQLs Table{..} = concat
      [ [sqlCreateTable tblName]
      , if null tblColumns
        then []
        else [sqlAlterTable tblName $ map sqlAddColumn tblColumns]
      , case tblPrimaryKey of
          Nothing -> []
          Just pk -> [sqlAlterTable tblName [sqlAddPK tblName pk]]
      , if null tblChecks
            then []
            else [sqlAlterTable tblName $ map sqlAddCheck tblChecks]
      , map (sqlCreateIndex tblName) tblIndexes
      ]

    normalizePropertyNames :: Table -> m ()
    normalizePropertyNames table@Table{..} = do
          kRun_ $ sqlGetPrimaryKey table
          pk <- kFold fetchPrimaryKey Nothing
          kRun_ $ sqlGetIndexes table
          indexes <- kFold fetchTableIndexes []
          kRun_ $ sqlGetForeignKeys table
          fkeys <- kFold fetchForeignKeys []
          let renames = concat [
                  normalizePK pk
                , concatMap normalizeIndex indexes
                , concatMap normalizeFK fkeys
                ]
          when (not . null $ renames) $ do
            logger $ arrListTable table ++ "normalizing property names..."
            forM_ renames $ logged kRun_
          where
            normalizePK :: Maybe (PrimaryKey, RawSQL) -> [SQL]
            normalizePK Nothing = []
            normalizePK (Just (_, name)) = case pkName tblName of
              pkname
                | pkname == raw name -> []
                | otherwise -> ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> pkname]

            normalizeIndex :: (TableIndex, RawSQL) -> [SQL]
            normalizeIndex (idx, name) = case indexName tblName idx of
              idxname
                | idxname == raw name -> []
                | otherwise -> ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> idxname]

            normalizeFK :: (ForeignKey, RawSQL) -> [SQL]
            normalizeFK (fk, name) = case fkName tblName fk of
              fkname
                | fkname == raw name -> []
                | otherwise -> [sqlAlterTable tblName [
                    "DROP CONSTRAINT" <+> raw name
                  , sqlAddFK tblName fk
                  ]
                ]

    checkTables :: m ([Table], [(Table, Int)])
    checkTables = (second catMaybes . partitionEithers) `liftM` mapM checkTable tables

    checkTable :: Table -> m (Either Table (Maybe (Table, Int)))
    checkTable table@Table{..} = do
      logger $ arrListTable table ++ "checking version..."
      mver <- checkTableVersion table
      case mver of
        Nothing -> do
          return $ Left table
        Just ver | ver == tblVersion -> do
          logger $ arrListTable table ++ "checking structure (v" ++ show ver ++ ")..."
          validation <- checkTableStructure table
          case validation of
            ValidationResult [] -> return $ Right Nothing
            ValidationResult errmsgs -> do
              mapM_ logger errmsgs
              error $ "Existing '" ++ tblNameString table ++ "' table structure is invalid."
        Just ver | ver < tblVersion -> do
          logger $ arrListTable table ++ "scheduling for migration: " ++ show ver ++ " => " ++ show tblVersion
          return . Right . Just $ (table, ver)
        Just ver -> error $ "Table '" ++ tblNameString table ++ "' in the database has higher version than the definition (database: " ++ show ver ++ ", definition: " ++ show tblVersion ++ ")"

    checkTableVersion :: Table -> m (Maybe Int)
    checkTableVersion table = do
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
          return Nothing

    migrate :: [Migration m] -> [(Table, Int)] -> m ()
    migrate ms ts = forM_ ms $ \Migration{..} -> forM_ ts $ \(t, from) -> do
      if tblName mgrTable == tblName t && mgrFrom >= from
         then do
           Just ver <- checkTableVersion mgrTable
           logger $ arrListTable mgrTable ++ "migrating: " ++ show ver ++ " => " ++ show (mgrFrom + 1) ++ "..."
           when (ver /= mgrFrom) $
             error $ "Migrations are in wrong order in migrations list."
           mgrDo
           kRun_ $ SQL "UPDATE table_versions SET version = ? WHERE name = ?"
                   [toSql . succ $ mgrFrom, toSql $ tblNameString t]
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
      kRun_ $ sqlGetPrimaryKey table
      pk <- kFold fetchPrimaryKey Nothing
      kRun_ $ sqlGetChecks table
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

        checkColumns :: Int -> [TableColumn] -> [TableColumn] -> ValidationResult
        checkColumns _ [] [] = mempty
        checkColumns _ rest [] = ValidationResult [tableHasLess "columns" rest]
        checkColumns _ [] rest = ValidationResult [tableHasMore "columns" rest]
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
            validateNames False = ValidationResult [errorMsg ("no. " ++ show n) "names" (unRawSQL . colName)]

            validateTypes True = mempty
            validateTypes False = ValidationResult [(errorMsg cname "types" (show . colType)) ++ " " ++ sql_hint ("TYPE" <+> columnTypeToSQL (colType d))]

            validateNullables True = mempty
            validateNullables False = ValidationResult [(errorMsg cname "nullables" (show . colNullable)) ++ " " ++ sql_hint ((if colNullable d then "DROP" else "SET") <+> "NOT NULL")]

            validateDefaults True = mempty
            validateDefaults False = ValidationResult [(errorMsg cname "defaults" (show . colDefault)) ++ " " ++ sql_hint set_default]
              where
                set_default = case colDefault d of
                  Just v  -> "SET DEFAULT" <+> v
                  Nothing -> "DROP DEFAULT"

            cname = unRawSQL . colName $ d
            errorMsg ident attr f = "Column '" ++ ident ++ "' differs in " ++ attr ++ " (table: " ++ f c ++ ", definition: " ++ f d ++ ")."
            sql_hint sql = "(HINT: SQL for making the change is: ALTER TABLE " ++ tblNameString table ++ " ALTER COLUMN " ++ unRawSQL (colName d) ++ " " ++ unRawSQL sql ++ ")"

        checkPrimaryKey :: Maybe PrimaryKey -> Maybe (PrimaryKey, RawSQL) -> ValidationResult
        checkPrimaryKey mdef mpk = mconcat [
            checkEquality "PRIMARY KEY" def (map fst pk)
          , checkNames (const (pkName tblName)) pk
          ]
          where
            def = maybeToList mdef
            pk = maybeToList mpk

        checkChecks :: [TableCheck] -> [TableCheck] -> ValidationResult
        checkChecks defs checks = case checkEquality "CHECKs" defs checks of
          ValidationResult [] -> ValidationResult []
          ValidationResult errmsgs -> ValidationResult $ errmsgs ++ [" (HINT: If checks are equal modulo number of parentheses/whitespaces used in conditions, just copy and paste expected output into source code)"]

        checkIndexes :: [TableIndex] -> [(TableIndex, RawSQL)] -> ValidationResult
        checkIndexes defs indexes = mconcat [
            checkEquality "INDEXes" defs (map fst indexes)
          , checkNames (indexName tblName) indexes
          ]

    checkForeignKeys :: Table -> m ()
    checkForeignKeys table@Table{..} = do
      logger $ arrListTable table ++ "checking foreign keys..."
      kRun_ $ sqlGetForeignKeys table
      fkeys <- kFold fetchForeignKeys []
      case validate tblForeignKeys fkeys of
        ValidationResult [] -> return ()
        ValidationResult errmsgs -> do
          mapM_ logger errmsgs
          error $ "Foreign keys on table '" ++ tblNameString table ++ "' are invalid."
      where
        validate :: [ForeignKey] -> [(ForeignKey, RawSQL)] -> ValidationResult
        validate defs fkeys = mconcat [
            checkEquality "FOREIGN KEYs" defs (map fst fkeys)
          , checkNames (fkName tblName) fkeys
          ]

    -- *** PRIMARY KEY ***

    sqlGetPrimaryKey :: Table -> SQL
    sqlGetPrimaryKey table = toSQLCommand . sqlSelect "pg_catalog.pg_constraint c" $ do
      sqlResult "c.conname"
      sqlResult "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = c.conrelid AND a.attnum = ANY (c.conkey)), ',') as columns" -- list of affected columns
      sqlWhereEq "c.contype" 'p'
      sqlWhereEqSql "c.conrelid" $ sqlGetTableID table

    fetchPrimaryKey :: Maybe (PrimaryKey, RawSQL) -> String -> String -> Maybe (PrimaryKey, RawSQL)
    fetchPrimaryKey Nothing name columns = (, unsafeFromString name)
      `liftM` (pkOnColumns . map unsafeFromString . split "," $ columns)
    fetchPrimaryKey _ _ _ =
      error $ "fetchPrimaryKey: more than one primary key (shouldn't happen)"

    -- *** CHECKS ***

    sqlGetChecks :: Table -> SQL
    sqlGetChecks table = toSQLCommand . sqlSelect "pg_catalog.pg_constraint c" $ do
      sqlResult "c.conname"
      sqlResult $  "regexp_replace(pg_get_constraintdef(c.oid, true), 'CHECK \\((.*)\\)', '\\1') as body" -- check body
      sqlWhereEq "c.contype" 'c'
      sqlWhereEqSql "c.conrelid" $ sqlGetTableID table

    fetchTableChecks :: [TableCheck] -> String -> String -> [TableCheck]
    fetchTableChecks acc name condition = TableCheck {
        chkName = unsafeFromString name
      , chkCondition = unsafeFromString condition
      } : acc

    -- *** INDEXES ***

    sqlGetIndexes :: Table -> SQL
    sqlGetIndexes table = toSQLCommand . sqlSelect "pg_catalog.pg_class c" $ do
      sqlResult "c.relname" -- index name
      sqlResult "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = i.indexrelid), ',')" -- array of affected columns
      sqlResult "i.indisunique" -- is it unique?
      sqlResult "CASE WHEN pg_get_indexdef(i.indexrelid, 0, true) LIKE '%WHERE%' THEN regexp_replace(pg_get_indexdef(i.indexrelid, 0, true), '.*WHERE (.*)', '\\1') ELSE NULL END" -- if partial, get constraint def
      sqlJoinOn "pg_catalog.pg_index i" "c.oid = i.indexrelid"
      sqlLeftJoinOn "pg_catalog.pg_constraint r" "r.conindid = i.indexrelid"
      sqlWhereEqSql "i.indrelid" $ sqlGetTableID table
      sqlWhereIsNULL "r.contype" -- fetch only "pure" indexes

    fetchTableIndexes :: [(TableIndex, RawSQL)] -> String -> String -> Bool -> Maybe String -> [(TableIndex, RawSQL)]
    fetchTableIndexes acc name columns unique mconstraint = (TableIndex {
        idxColumns = map unsafeFromString . split "," $ columns
      , idxUnique = unique
      , idxWhere = unsafeFromString `liftM` mconstraint
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

    -- *** UTILS ***

    checkEquality :: (Eq t, Show t) => String -> [t] -> [t] -> ValidationResult
    checkEquality pname defs props = case (defs L.\\ props, props L.\\ defs) of
      ([], []) -> mempty
      (def_diff, db_diff) -> ValidationResult [concat [
          "Table and its definition have diverged and have "
        , show $ length db_diff
        , " and "
        , show $ length def_diff
        , " different "
        , pname
        , " each, respectively (table: "
        , show db_diff
        , ", definition: "
        , show def_diff
        , ")."
        ]]

    checkNames :: Show t => (t -> SQL) -> [(t, RawSQL)] -> ValidationResult
    checkNames prop_name = mconcat . map check
      where
        check (prop, name) = case prop_name prop of
          pname
            | pname == raw name -> mempty
            | otherwise -> ValidationResult [ concat [
                "Property "
              , show prop
              , " has invalid name (expected: "
              , show pname
              , ", given: "
              , show (raw name)
              , ")."
              ]]

    tableHasLess :: Show t => String -> t -> String
    tableHasLess ptype missing = "Table in the database has *less* " ++ ptype ++ " than its definition (missing: " ++ show missing ++ ")"

    tableHasMore :: Show t => String -> t -> String
    tableHasMore ptype extra = "Table in the database has *more* " ++ ptype ++ " than its definition (extra: " ++ show extra ++ ")"

    arrListTable :: Table -> String
    arrListTable table = " -> " ++ tblNameString table ++ ": "
