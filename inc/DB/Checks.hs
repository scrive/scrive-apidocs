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
  checkDBConsistency logger (tableVersions : tables) migrations
  -- everything is OK, commit changes
  kCommit
  return ()

-- | Return SQL fragment of current catalog within quotes
currentCatalog :: MonadDB m => m RawSQL
currentCatalog = do
  Just dbname <- getOne "SELECT current_catalog"
  return $ unsafeFromString $ "\"" ++ dbname ++ "\""

-- |  Checks whether database returns timestamps in UTC
checkDBTimeZone :: MonadDB m => (String -> m ()) -> m ()
checkDBTimeZone logger = do
  dbname <- currentCatalog
  logger $ "Setting " ++ unRawSQL dbname ++ " database to return timestamps in UTC"
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

-- | Checks whether database is consistent (performs migrations if necessary)
checkDBConsistency :: MonadDB m => (String -> m ()) -> [Table] -> [Migration m] -> m ()
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
    checkTable table@Table{..} = do
      logger $ "Checking table '" ++ tblNameString table ++ "'..."
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
        sqlWhereEqSql "a.attrelid" sqlGetTableID
      desc <- reverse `liftM` kFold fetchTableColumn []
      if null desc
        then do
          logger "Table doesn't exist, creating..."
          kRun_ createStatement
          logger $ "Table created, writing version information..."
          kRun_ . sqlInsert "table_versions" $ do
            sqlSet "name" tblName
            sqlSet "version" tblVersion
          _ <- checkTable table
          return $ Left table
        else do
          -- get info about constraints from pg_catalog
          kRun_ $ sqlGetConstraintOfType 'p' -- primary key
          pk <- kFold fetchTablePrimaryKey Nothing
          kRun_ $ sqlGetConstraintOfType 'u' -- uniques
          uniques <- kFold fetchTableUniques []
          kRun_ $ sqlGetConstraintOfType 'c' -- checks
          checks <- kFold fetchTableChecks []
          let validation = mconcat [
                  checkColumns 0 tblColumns desc
                , checkPrimaryKey tblPrimaryKey pk
                , checkUniques tblUniques uniques
                , checkChecks tblChecks checks
                ]
          case validation of
            ValidationError errmsg -> do
              logger errmsg
              logger "Table structure is invalid, checking version..."
              ver <- checkVersion table
              if ver == tblVersion
                then do
                  error $ "Existing '" ++ tblNameString table ++ "' table structure is invalid."
                else do
                  logger "Table is outdated, scheduling for migration."
                  return $ Right $ Just (table, ver)
            ValidationOk sqls -> do
              if null sqls
                then logger "Table structure is valid."
                else do
                  logger $ show sqls
                  logger $ "Correcting structure..."
                  mapM_ kRun_ sqls
              logger "Checking table version..."
              ver <- checkVersion table
              if ver == tblVersion
                then do
                  logger "Version of table in application matches database version."
                  return $ Right Nothing
                else do
                  logger $ "Versions differ (application: " ++ show tblVersion ++ ", database: " ++ show ver ++ "), scheduling for migration."
                  return $ Right $ Just (table, ver)
      where
        sqlGetTableID = parenthesize . toSQLCommand $
          sqlSelect "pg_catalog.pg_class c" $ do
            sqlResult "c.oid"
            sqlLeftJoinOn "pg_catalog.pg_namespace n" "n.oid = c.relnamespace"
            sqlWhereEq "c.relname" $ tblNameString table
            sqlWhere "pg_catalog.pg_table_is_visible(c.oid)"

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

        genPrimaryKeyName = "pk_" <> raw tblName
        genUniqueName cols = "unique_" <> raw tblName <> "_" <> intersperseNoWhitespace "_" (map raw cols)
        genCheckName TableCheck{chkName} = "check_" <> raw tblName <> "_" <> raw chkName

        columnToSQL TableColumn{..} = raw $ colName
          <+> colTypeToSQL colType
          <+> if colNullable then "NULL" else "NOT NULL"
          <+> maybe "" ("DEFAULT" <+>) colDefault
          where
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

        createStatement = mconcat [
            raw $ "CREATE TABLE" <+> tblName <+> "("
          , intersperse ", " $ map columnToSQL tblColumns
          , if primaryKeyToSQL == mempty then "" else ", "
          , primaryKeyToSQL
          , if uniquesToSQL == mempty then "" else ", "
          , uniquesToSQL
          , if checksToSQL == mempty then "" else ", "
          , checksToSQL
          , ")"
          ]
          where
            uniquesToSQL = intersperse ", " $ map uniqueToSQL tblUniques
            checksToSQL = intersperse ", " $ map checkToSQL tblChecks

        primaryKeyToSQL = "CONSTRAINT"
          <+> genPrimaryKeyName
          <+> "PRIMARY KEY ("
          <+> intersperse ", " (map raw tblPrimaryKey)
          <+> ")"

        uniqueToSQL unique = "CONSTRAINT"
            <+> genUniqueName unique
            <+> "UNIQUE ("
            <+> intersperse ", " (map raw unique)
            <+> ")"

        checkToSQL chk@TableCheck{..} = "CONSTRAINT"
            <+> genCheckName chk
            <+> "CHECK ("
            <+> raw chkCondition
            <+> ")"

        sqlGetConstraintOfType :: Char -> SQL
        sqlGetConstraintOfType ctype = toSQLCommand $
          sqlSelect "pg_catalog.pg_constraint c" $ do
            sqlResult "c.conname"
            sqlResult $ if ctype == 'c'
              then "regexp_replace(pg_get_constraintdef(c.oid, true), 'CHECK \\((.*)\\)', '\\1')" -- check body
              else "array_to_string(array(SELECT a.attname FROM pg_catalog.pg_attribute a WHERE a.attrelid = c.conrelid AND a.attnum = ANY (c.conkey)), ',')" -- list of affected columns (unique, primary key)
            sqlWhereEq "c.contype" ctype
            sqlWhereEqSql "c.conrelid" sqlGetTableID

        checkColumns :: Int -> [TableColumn] -> [TableColumn] -> ValidationResult
        checkColumns _ [] [] = mempty
        checkColumns _ rest [] = ValidationOk $ map (\col -> "ALTER TABLE" <+> raw tblName <+> "ADD COLUMN" <+> columnToSQL col) rest
        checkColumns !n [] rest = ValidationError $ "Table in database has too many columns (definition: " ++ show n ++ ", database: " ++ show (n + length rest) ++ ")"
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
            validateTypes False = ValidationError $ errorMsg cname "types" (show . colType)
            validateNullables True = mempty
            validateNullables False = ValidationOk ["ALTER TABLE" <+> raw tblName <+> "ALTER COLUMN" <+> raw (colName d) <+> (if colNullable d then "DROP" else "SET") <+> "NOT NULL"]
            validateDefaults True = mempty
            validateDefaults False = ValidationOk ["ALTER TABLE" <+> raw tblName <+> "ALTER COLUMN" <+> raw (colName d) <+> set_default]
              where
                set_default = case colDefault d of
                  Just v -> "SET DEFAULT" <+> raw v
                  Nothing -> "DROP DEFAULT"

            cname = "'" ++ unRawSQL (colName d) ++ "'"
            errorMsg ident attr f = "Column " ++ ident ++ " differs in " ++ attr ++ " (definition: " ++ f d ++ ", database: " ++ f c ++ ")"

        checkPrimaryKey :: [RawSQL] -> Maybe (RawSQL, Set RawSQL) -> ValidationResult
        checkPrimaryKey [] Nothing = mempty
        checkPrimaryKey _ Nothing = ValidationOk ["ALTER TABLE " <+> raw tblName <+> "ADD" <+> primaryKeyToSQL]
        checkPrimaryKey [] (Just (_, cols)) = ValidationError $ "No primary key in the definition, but key present in the database on columns: " ++ L.intercalate ", " (map unRawSQL $ S.toList cols)
        checkPrimaryKey def (Just (name, cols))
          | S.fromList def == cols = ValidationOk $ if raw name /= genPrimaryKeyName
            -- rename index that represents primary key
            then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> genPrimaryKeyName]
            else []
          | otherwise = ValidationError $ "Primary key in table definition (" ++ L.intercalate ", " (map unRawSQL def) ++ ") differs from the one in the database (" ++ L.intercalate ", " (map unRawSQL $ S.toList cols) ++ ")"

        checkUniques :: [[RawSQL]] -> [(Set RawSQL, RawSQL)] -> ValidationResult
        checkUniques [] [] = mempty
        checkUniques rest [] = ValidationOk $ map (\unique -> "ALTER TABLE" <+> raw tblName <+> "ADD CONTRAINT" <+> uniqueToSQL unique) rest
        checkUniques [] rest = ValidationError $ "Table in database has more UNIQUE constraints than definition (" ++ L.intercalate ", " (map (\(cols, name) -> unRawSQL name ++ "[" ++ L.intercalate ", " (map unRawSQL $ S.toList cols) ++ "]") rest) ++ ")"
        checkUniques (d:defs) uniques = mconcat [
            case sd `L.lookup` uniques of
              Nothing -> mempty
              Just name -> ValidationOk $ if raw name /= genUniqueName d
                then ["ALTER INDEX" <+> raw name <+> "RENAME TO" <+> genUniqueName d]
                else []
          , checkUniques defs new_uniques
          ]
          where
            sd = S.fromList d
            new_uniques = deleteFirst ((== sd) . fst) uniques

        checkChecks :: [TableCheck] -> [(RawSQL, RawSQL)] -> ValidationResult
        checkChecks [] [] = mempty
        checkChecks rest [] = ValidationOk $ map (\chk -> "ALTER TABLE" <+> raw tblName <+> "ADD" <+> checkToSQL chk) rest
        checkChecks [] rest = ValidationError $ "Table in database has more CHECK constraints that definition (" ++ L.intercalate ", " (map (\(condition, name) -> unRawSQL name ++ "[" ++ unRawSQL condition ++ "]") rest) ++ ")"
        checkChecks (d:defs) checks = mconcat [
            case chkCondition d `L.lookup` checks of
              Just name -> ValidationOk $ if genCheckName d /= raw name
                -- remove old check and add new, there is no way to rename it
                then  ["ALTER TABLE" <+> raw tblName <+> "DROP CONSTRAINT" <+> raw name, "ALTER TABLE" <+> raw tblName <+> "ADD" <+> checkToSQL d]
                else []
              -- it may happen that the body is different, but there already is
              -- a check with that name. so let's check if there exist a check
              -- with the same name, but different condition and display
              -- appropriate error message if that's tha case.
              Nothing -> case filter ((== genCheckName d) . raw . snd) checks of
                []  -> mempty
                [(condition, name)] -> ValidationError $ "Check " ++ unRawSQL name ++ " in database has different condition [" ++ unRawSQL condition ++ "] than the one in the definition [" ++ unRawSQL (chkCondition d) ++ "]"
                _ -> error "checkChecks: more that one CHECK with the same name in the definition (shouldn't happen)"

          , checkChecks defs new_checks
          ]
          where
            new_checks = deleteFirst ((== chkCondition d) . fst) checks

        deleteFirst :: (a -> Bool) -> [a] -> [a]
        deleteFirst f xs = let (a, b) = break f xs in a ++ drop 1 b

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
           kRun_ $ SQL "UPDATE table_versions SET version = ? WHERE name = ?"
                   [toSql $ succ $ mgrFrom m, toSql $ tblName t]
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
      -- This method ignores all specific data on an index:
      -- partiality, uniqueness, expressions or other.. Support for
      -- advanced constructs is left for later implementation.
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

      present <- kFold fetchNamedIndex []

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

      fkPresent <- kFold fetchNamedForeignKey []

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
