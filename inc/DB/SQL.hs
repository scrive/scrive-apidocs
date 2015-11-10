{-# LANGUAGE ExistentialQuantification #-}
{- |

Module SQL2 offers some nice monadic function that build SQL commands
on the fly. Some examples:

> kRun_ $ sqlSelect "documents" $ do
>   sqlResult "id"
>   sqlResult "title"
>   sqlResult "mtime"
>   sqlOrderBy "documents.mtime DESC"
>   sqlWhereILike "documents.title" pattern

SQL2 supports SELECT as 'sqlSelect' and data manipulation using
'sqlInsert', 'sqlInsertSelect', 'sqlDelete' and 'sqlUpdate'.

> kRun_ $ sqlInsert "documents" $ do
>   sqlSet "title" title
>   sqlSet "ctime" now
>   sqlResult "id"

The 'sqlInsertSelect' is particulary interesting as it supports INSERT
of values taken from a SELECT clause from same or even different
tables.

There is a possibility to do multiple inserts at once. Data given by
'sqlSetList' will be inserted multiple times, data given by 'sqlSet'
will be multiplied as many times as needed to cover all inserted rows
(it is common to all rows). If you use multiple 'sqlSetList' then
lists will be made equal in length by appending @DEFAULT@ as fill
element.

> kRun_ $ sqlInsert "documents" $ do
>   sqlSet "ctime" now
>   sqlSetList "title" [title1, title2, title3]
>   sqlResult "id"

The above will insert 3 new documents.

SQL2 provides quite a lot of SQL magic, including @ORDER BY@ as
'sqlOrderBy', @GROUP BY@ as 'sqlGroupBy'.

> kRun_ $ sqlSelect "documents" $ do
>   sqlResult "id"
>   sqlResult "title"
>   sqlResult "mtime"
>   sqlOrderBy "documents.mtime DESC"
>   sqlOrderBy "documents.title"
>   sqlGroupBy "documents.status"
>   sqlJoinOn "users" "documents.user_id = users.id"
>   sqlWhere $ SQL "documents.title ILIKE ?" [toSql pattern]

Joins are done by 'sqlJoinOn', 'sqlLeftJoinOn', 'sqlRightJoinOn',
'sqlJoinOn', 'sqlFullJoinOn'. If everything fails use 'sqlJoin' and
'sqlFrom' to set join clause as string. Support for a join grammars as
some kind of abstract syntax data type is lacking.

> kRun_ $ sqlDelete "mails" $ do
>   sqlWhere "id > 67"

> kRun_ $ sqlUpdate "document_tags" $ do
>   sqlSet "value" (123 :: Int)
>   sqlWhere "name = 'abc'"

Exception returning and 'kWhyNot' are a subsystem for querying why a
query did not provide expected results. For example:

> let query = sqlUpdate "documents" $ do
>               sqlSet "deleted" True
>               sqlWhereEq "documents.id" 12345
>               sqlWhereEqE DocumentDeleteFlagMustBe "documents.deleted" False
>               sqlWhereILikeE DocumentTitleMustContain "documents.title" "%important%"
> result <- kRun query

In result is zero then no document was updated. We would like to know
what happened. In query we have three filtering clauses. One is a
baseline: the one mentioning @documents.id@. Baseline clauses define
what objects are we talking about. Other clauses are correctness
checks and may fail if status of on object is unexpected. Using
'kWhyNot' we can see what is wrong with an object:

> problems <- kWhyNot query

Now problems should contain a list of issues with rows that could be
possibly be affected by weren't due to correctness clauses. For
example it may state:

> problems = [[ DocumentDeleteFlagMustBe { documentDeleteFlagMustBe = False
>                                        , documentDeleteFlagReallyIs = True
>                                        }
>             , DocumentTitleMustContain { documentTitleMustContain = "%important%"
>                                        , documentTitleReallyIs = "Some contract v2"
>                                        }
>             ]]

Note: problems is a nested array, for each object we get a list of
issues pertaining to that object. If that list is empty, then it means
that baseline conditions failed or there is no such object that
fullfills all conditions at the same time although there are some that
fullfill each one separatelly.

-}

-- TODO: clean this up and fix the mess with
-- "randomly" wrapping stuff in parentheses.

module DB.SQL
  ( sqlWhere
  , sqlWhereE
  , sqlWhereEV
  , sqlWhereEVV
  , sqlWhereEVVV
  , sqlWhereEVVVV
  , sqlWhereEq
  , sqlWhereEqE
  , sqlWhereEqSql
  , sqlWhereNotEq
  , sqlWhereNotEqE
  , sqlWhereIn
  , sqlWhereInSql
  , sqlWhereInE
  , sqlWhereNotIn
  , sqlWhereNotInSql
  , sqlWhereNotInE
  , sqlWhereExists
  , sqlWhereNotExists
  , sqlWhereLike
  , sqlWhereLikeE
  , sqlWhereILike
  , sqlWhereILikeE
  , sqlWhereIsNULL
  , sqlWhereIsNotNULL
  , sqlWhereIsNULLE

  , sqlIgnore

  , sqlFrom
  , sqlJoin
  , sqlJoinOn
  , sqlLeftJoinOn
  , sqlRightJoinOn
  , sqlFullJoinOn
  , sqlSet
  , sqlSetInc
  , sqlSetList
  , sqlSetListWithDefaults
  , sqlSetCmd
  , sqlSetCmdList
  , sqlCopyColumn
  , sqlResult
  , sqlOrderBy
  , sqlGroupBy
  , sqlHaving
  , sqlOffset
  , sqlLimit
  , sqlDistinct
  , sqlWith

  , SqlTurnIntoSelect
  , sqlTurnIntoSelect
  , sqlTurnIntoWhyNotSelect

  , sqlSelect
  , sqlSelect2
  , SqlSelect(..)
  , sqlInsert
  , SqlInsert(..)
  , sqlInsertSelect
  , SqlInsertSelect(..)
  , sqlUpdate
  , SqlUpdate(..)
  , sqlDelete
  , SqlDelete(..)

  , sqlWhereAny

  , SqlResult
  , SqlSet
  , SqlFrom
  , SqlWhere
  , SqlOrderBy
  , SqlGroupByHaving
  , SqlOffsetLimit
  , SqlDistinct


  , SqlCondition(..)
  , sqlGetWhereConditions

  , SqlWhyNot(..)

  , kWhyNot1
  --, DBExceptionCouldNotParseValues(..)
  , kRun1OrThrowWhyNot
  , kRun1OrThrowWhyNotAllowIgnore
  , kRunManyOrThrowWhyNot
  , kRunAndFetch1OrThrowWhyNot

  , KontraException(..)
  , SomeKontraException(..)
  , catchKontra
  , DBBaseLineConditionIsFalse(..)

  , Sqlable(..)
  , sqlOR
  , sqlConcatComma
  , sqlConcatAND
  , sqlConcatOR
  , parenthesize
  , AscDesc(..)
  )
  where

import Control.Exception.Lifted as E
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.String
import Data.Typeable
import Database.PostgreSQL.PQTypes
import qualified Text.JSON.Gen as JSON

import KontraPrelude

class Sqlable a where
  toSQLCommand :: a -> SQL

instance Sqlable SQL where
  toSQLCommand = id

smintercalate :: (IsString m, Monoid m) => m -> [m] -> m
smintercalate m = mintercalate $ mconcat [mspace, m, mspace]

sqlOR :: SQL -> SQL -> SQL
sqlOR s1 s2 = sqlConcatOR [s1, s2]

sqlConcatComma :: [SQL] -> SQL
sqlConcatComma = mintercalate ", "

sqlConcatAND :: [SQL] -> SQL
sqlConcatAND = smintercalate "AND" . map parenthesize

sqlConcatOR :: [SQL] -> SQL
sqlConcatOR = smintercalate "OR" . map parenthesize

parenthesize :: SQL -> SQL
parenthesize s = "(" <> s <> ")"

-- | 'AscDesc' marks ORDER BY order as ascending or descending.
-- Conversion to SQL adds DESC marker to descending and no marker
-- to ascending order.
data AscDesc a = Asc a | Desc a

data Multiplicity a = Single a | Many [a]
  deriving (Eq, Ord, Show, Typeable)

-- | 'SqlCondition' are clauses that are in SQL statements in the
-- WHERE block. Each statement has a list of conditions, all of them
-- must be fullfilled.  Sometimes we need to inspect internal
-- structure of a condition. For now it seems that the only
-- interesting case is EXISTS (SELECT ...) because that internal
-- SELECT can have explainable clauses.
data SqlCondition = SqlPlainCondition SQL SqlWhyNot
                  | SqlExistsCondition SqlSelect
                    deriving (Typeable, Show)

-- | 'SqlWhyNot' contains recepie how to query the database for
-- current values in there and construct proper exception object using
-- that information. For @SqlWhyNot mkException queries@ the
-- @mkException@ should take as input same lenth list as there are
-- queries. Each query will be run in a JOIN context with all
-- referenced tables, so it can extract values from there.
data SqlWhyNot = forall e row. (FromRow row, KontraException e) => SqlWhyNot Bool (row -> e) [SQL]

{-
instance Eq SqlCondition where
  (SqlPlainCondition a _) == (SqlPlainCondition b _) = a == b
  (SqlExistsCondition a) == (SqlExistsCondition b) = a == b
  _ == _ = False
  -}

instance Show SqlWhyNot where
  show (SqlWhyNot _important exc expr) = "SqlWhyNot " ++ show (typeOf (exc $undefined)) ++ " " ++ show expr

instance Sqlable SqlCondition where
  toSQLCommand (SqlPlainCondition a _) = a
  toSQLCommand (SqlExistsCondition a) = "EXISTS (" <> toSQLCommand (a { sqlSelectResult = ["TRUE"] }) <> ")"

data SqlSelect = SqlSelect
  { sqlSelectFrom    :: SQL
  , sqlSelectDistinct :: Bool
  , sqlSelectResult  :: [SQL]
  , sqlSelectWhere   :: [SqlCondition]
  , sqlSelectOrderBy :: [SQL]
  , sqlSelectGroupBy :: [SQL]
  , sqlSelectHaving  :: [SQL]
  , sqlSelectOffset  :: Integer
  , sqlSelectLimit   :: Integer
  , sqlSelectWith    :: [(SQL, SQL)]
  }

data SqlUpdate = SqlUpdate
  { sqlUpdateWhat    :: SQL
  , sqlUpdateFrom    :: SQL
  , sqlUpdateWhere   :: [SqlCondition]
  , sqlUpdateSet     :: [(SQL,SQL)]
  , sqlUpdateResult  :: [SQL]
  , sqlUpdateWith    :: [(SQL, SQL)]
  }

data SqlInsert = SqlInsert
  { sqlInsertWhat    :: SQL
  , sqlInsertSet     :: [(SQL, Multiplicity SQL)]
  , sqlInsertResult  :: [SQL]
  , sqlInsertWith    :: [(SQL, SQL)]
  }

data SqlInsertSelect = SqlInsertSelect
  { sqlInsertSelectWhat    :: SQL
  , sqlInsertSelectDistinct :: Bool
  , sqlInsertSelectSet     :: [(SQL, SQL)]
  , sqlInsertSelectResult  :: [SQL]
  , sqlInsertSelectFrom    :: SQL
  , sqlInsertSelectWhere   :: [SqlCondition]
  , sqlInsertSelectOrderBy :: [SQL]
  , sqlInsertSelectGroupBy :: [SQL]
  , sqlInsertSelectHaving  :: [SQL]
  , sqlInsertSelectOffset  :: Integer
  , sqlInsertSelectLimit   :: Integer
  , sqlInsertSelectWith    :: [(SQL, SQL)]
  }

data SqlDelete = SqlDelete
  { sqlDeleteFrom    :: SQL
  , sqlDeleteUsing   :: SQL
  , sqlDeleteWhere   :: [SqlCondition]
  , sqlDeleteResult  :: [SQL]
  , sqlDeleteWith    :: [(SQL, SQL)]
  }

-- | This is not exported and is used as an implementation detail in
-- 'sqlWhereAll'.
data SqlAll = SqlAll
  { sqlAllWhere :: [SqlCondition]
  }

instance Show SqlSelect where
  show = show . toSQLCommand

instance Show SqlInsert where
  show = show . toSQLCommand

instance Show SqlInsertSelect where
  show = show . toSQLCommand

instance Show SqlUpdate where
  show = show . toSQLCommand

instance Show SqlDelete where
  show = show . toSQLCommand

instance Show SqlAll where
  show = show . toSQLCommand

emitClause :: Sqlable sql => SQL -> sql -> SQL
emitClause name s = case toSQLCommand s of
  sql
    | isSqlEmpty sql -> ""
    | otherwise   -> name <+> sql

emitClausesSep :: SQL -> SQL -> [SQL] -> SQL
emitClausesSep _name _sep [] = mempty
emitClausesSep name sep sqls = name <+> smintercalate sep (filter (not . isSqlEmpty) $ map parenthesize sqls)

emitClausesSepComma :: SQL -> [SQL] -> SQL
emitClausesSepComma _name [] = mempty
emitClausesSepComma name sqls = name <+> sqlConcatComma (filter (not . isSqlEmpty) sqls)

instance IsSQL SqlSelect where
  withSQL = withSQL . toSQLCommand

instance IsSQL SqlInsert where
  withSQL = withSQL . toSQLCommand

instance IsSQL SqlInsertSelect where
  withSQL = withSQL . toSQLCommand

instance IsSQL SqlUpdate where
  withSQL = withSQL . toSQLCommand

instance IsSQL SqlDelete where
  withSQL = withSQL . toSQLCommand

instance Sqlable SqlSelect where
  toSQLCommand cmd =
        emitClausesSepComma "WITH" (map (\(name,command) -> name <+> "AS" <+> parenthesize command) (sqlSelectWith cmd)) <+>
        "SELECT" <+> (if sqlSelectDistinct cmd then "DISTINCT" else mempty) <+>
        sqlConcatComma (sqlSelectResult cmd) <+>
        emitClause "FROM" (sqlSelectFrom cmd) <+>
        emitClausesSep "WHERE" "AND" (map toSQLCommand $ sqlSelectWhere cmd) <+>
        emitClausesSepComma "GROUP BY" (sqlSelectGroupBy cmd) <+>
        emitClausesSep "HAVING" "AND" (sqlSelectHaving cmd) <+>
        emitClausesSepComma "ORDER BY" (sqlSelectOrderBy cmd) <+>
        (if sqlSelectOffset cmd > 0
           then unsafeSQL ("OFFSET " ++ show (sqlSelectOffset cmd))
           else "") <+>
        (if sqlSelectLimit cmd >= 0
           then unsafeSQL ("LIMIT " ++ show (sqlSelectLimit cmd))
           else "")

instance Sqlable SqlInsert where
  toSQLCommand cmd =
    emitClausesSepComma "WITH" (map (\(name,command) -> name <+> "AS" <+> parenthesize command) (sqlInsertWith cmd)) <+>
    "INSERT INTO" <+> sqlInsertWhat cmd <+>
    parenthesize (sqlConcatComma (map fst (sqlInsertSet cmd))) <+>
    emitClausesSep "VALUES" "," (map sqlConcatComma (transpose (map (makeLongEnough . snd) (sqlInsertSet cmd)))) <+>
    emitClausesSepComma "RETURNING" (sqlInsertResult cmd)
   where
      -- this is the longest list of values
      longest = $maximum (1 : (map (lengthOfEither . snd) (sqlInsertSet cmd)))
      lengthOfEither (Single _) = 1
      lengthOfEither (Many x) = length x
      makeLongEnough (Single x) = take longest (repeat x)
      makeLongEnough (Many x) = take longest (x ++ repeat "DEFAULT")

instance Sqlable SqlInsertSelect where
  toSQLCommand cmd =
    "INSERT INTO" <+> sqlInsertSelectWhat cmd <+>
    parenthesize (sqlConcatComma (map fst (sqlInsertSelectSet cmd))) <+>
    parenthesize (toSQLCommand (SqlSelect { sqlSelectFrom    = sqlInsertSelectFrom cmd
                                          , sqlSelectDistinct = sqlInsertSelectDistinct cmd
                                          , sqlSelectResult  = fmap snd $ sqlInsertSelectSet cmd
                                          , sqlSelectWhere   = sqlInsertSelectWhere cmd
                                          , sqlSelectOrderBy = sqlInsertSelectOrderBy cmd
                                          , sqlSelectGroupBy = sqlInsertSelectGroupBy cmd
                                          , sqlSelectHaving  = sqlInsertSelectHaving cmd
                                          , sqlSelectOffset  = sqlInsertSelectOffset cmd
                                          , sqlSelectLimit   = sqlInsertSelectLimit cmd
                                          , sqlSelectWith    = sqlInsertSelectWith cmd
                                          })) <+>
    emitClausesSepComma "RETURNING" (sqlInsertSelectResult cmd)

instance Sqlable SqlUpdate where
  toSQLCommand cmd =
    emitClausesSepComma "WITH" (map (\(name,command) -> name <+> "AS" <+> parenthesize command) (sqlUpdateWith cmd)) <+>
    "UPDATE" <+> sqlUpdateWhat cmd <+> "SET" <+>
    sqlConcatComma (map (\(name, command) -> name <> "=" <> command) (sqlUpdateSet cmd)) <+>
    emitClause "FROM" (sqlUpdateFrom cmd) <+>
    emitClausesSep "WHERE" "AND" (map toSQLCommand $ sqlUpdateWhere cmd) <+>
    emitClausesSepComma "RETURNING" (sqlUpdateResult cmd)

instance Sqlable SqlDelete where
  toSQLCommand cmd =
    emitClausesSepComma "WITH" (map (\(name,command) -> name <+> "AS" <+> parenthesize command) (sqlDeleteWith cmd)) <+>
    "DELETE FROM" <+> sqlDeleteFrom cmd <+>
    emitClause "USING" (sqlDeleteUsing cmd) <+>
        emitClausesSep "WHERE" "AND" (map toSQLCommand $ sqlDeleteWhere cmd) <+>
    emitClausesSepComma "RETURNING" (sqlDeleteResult cmd)

instance Sqlable SqlAll where
  toSQLCommand cmd | null (sqlAllWhere cmd) = "TRUE"
  toSQLCommand cmd =
    "(" <+> smintercalate "AND" (map (parenthesize . toSQLCommand) (sqlAllWhere cmd)) <+> ")"


sqlSelect :: SQL -> State SqlSelect () -> SqlSelect
sqlSelect table refine =
  execState refine (SqlSelect table False [] [] [] [] [] 0 (-1) [])

sqlSelect2 :: SQL -> State SqlSelect () -> SqlSelect
sqlSelect2 from refine =
  execState refine (SqlSelect from False [] [] [] [] [] 0 (-1) [])

sqlInsert :: SQL -> State SqlInsert () -> SqlInsert
sqlInsert table refine =
  execState refine (SqlInsert table mempty [] [])

sqlInsertSelect :: SQL -> SQL -> State SqlInsertSelect () -> SqlInsertSelect
sqlInsertSelect table from refine =
  execState refine (SqlInsertSelect
                    { sqlInsertSelectWhat    = table
                    , sqlInsertSelectDistinct = False
                    , sqlInsertSelectSet     = []
                    , sqlInsertSelectResult  = []
                    , sqlInsertSelectFrom    = from
                    , sqlInsertSelectWhere   = []
                    , sqlInsertSelectOrderBy = []
                    , sqlInsertSelectGroupBy = []
                    , sqlInsertSelectHaving  = []
                    , sqlInsertSelectOffset  = 0
                    , sqlInsertSelectLimit   = -1
                    , sqlInsertSelectWith    = []
                    })

sqlUpdate :: SQL -> State SqlUpdate () -> SqlUpdate
sqlUpdate table refine =
  execState refine (SqlUpdate table mempty [] [] [] [])

sqlDelete :: SQL -> State SqlDelete () -> SqlDelete
sqlDelete table refine =
  execState refine (SqlDelete  { sqlDeleteFrom   = table
                               , sqlDeleteUsing  = mempty
                               , sqlDeleteWhere  = []
                               , sqlDeleteResult = []
                               , sqlDeleteWith   = []
                               })

class SqlWith a where
  sqlWith1 :: a -> SQL -> SQL -> a


instance SqlWith SqlSelect where
  sqlWith1 cmd name sql = cmd { sqlSelectWith = sqlSelectWith cmd ++ [(name,sql)] }

instance SqlWith SqlInsertSelect where
  sqlWith1 cmd name sql = cmd { sqlInsertSelectWith = sqlInsertSelectWith cmd ++ [(name,sql)] }

instance SqlWith SqlUpdate where
  sqlWith1 cmd name sql = cmd { sqlUpdateWith = sqlUpdateWith cmd ++ [(name,sql)] }

instance SqlWith SqlDelete where
  sqlWith1 cmd name sql = cmd { sqlDeleteWith = sqlDeleteWith cmd ++ [(name,sql)] }

sqlWith :: (MonadState v m, SqlWith v, Sqlable s) => SQL -> s -> m ()
sqlWith name sql = modify (\cmd -> sqlWith1 cmd name (toSQLCommand sql))



class SqlWhere a where
  sqlWhere1 :: a -> SqlCondition -> a
  sqlGetWhereConditions :: a -> [SqlCondition]

instance SqlWhere SqlSelect where
  sqlWhere1 cmd cond = cmd { sqlSelectWhere = sqlSelectWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlSelectWhere

instance SqlWhere SqlInsertSelect where
  sqlWhere1 cmd cond = cmd { sqlInsertSelectWhere = sqlInsertSelectWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlInsertSelectWhere

instance SqlWhere SqlUpdate where
  sqlWhere1 cmd cond = cmd { sqlUpdateWhere = sqlUpdateWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlUpdateWhere

instance SqlWhere SqlDelete where
  sqlWhere1 cmd cond = cmd { sqlDeleteWhere = sqlDeleteWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlDeleteWhere

instance SqlWhere SqlAll where
  sqlWhere1 cmd cond = cmd { sqlAllWhere = sqlAllWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlAllWhere

newtype SqlWhereIgnore a = SqlWhereIgnore { unSqlWhereIgnore :: a }


ignoreWhereClause :: SqlCondition -> SqlCondition
ignoreWhereClause (SqlPlainCondition sql (SqlWhyNot _b f s)) =
  SqlPlainCondition sql (SqlWhyNot False f s)
ignoreWhereClause (SqlExistsCondition sql) =
  SqlExistsCondition (sql { sqlSelectWhere = map ignoreWhereClause (sqlSelectWhere sql)})

instance (SqlWhere a) => SqlWhere (SqlWhereIgnore a) where
  sqlWhere1 (SqlWhereIgnore cmd) cond =
        SqlWhereIgnore (sqlWhere1 cmd (ignoreWhereClause cond))
  sqlGetWhereConditions (SqlWhereIgnore cmd) = sqlGetWhereConditions cmd


sqlIgnore :: (MonadState s m)
          => State (SqlWhereIgnore s) a
          -> m ()
sqlIgnore clauses = modify (\cmd -> unSqlWhereIgnore (execState clauses (SqlWhereIgnore cmd)))

sqlWhere :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhere sql = sqlWhereE (DBBaseLineConditionIsFalse sql) sql

sqlWhereE :: (MonadState v m, SqlWhere v, KontraException e) => e -> SQL -> m ()
sqlWhereE exc sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (SqlWhyNot True exc2 [])))
  where
    exc2 (_::()) = exc

sqlWhereEV :: (MonadState v m, SqlWhere v, KontraException e, Show a, FromSQL a) => (a -> e, SQL) -> SQL -> m ()
sqlWhereEV (exc, vsql) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (SqlWhyNot True exc2 [vsql])))
  where
    exc2 (Identity v1) = exc v1

sqlWhereEVV :: (MonadState v m, SqlWhere v, KontraException e, FromSQL a, FromSQL b) => (a -> b -> e, SQL, SQL) -> SQL -> m ()
sqlWhereEVV (exc, vsql1, vsql2) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (SqlWhyNot True exc2 [vsql1, vsql2])))
  where
    exc2 (v1, v2) = exc v1 v2

sqlWhereEVVV :: (MonadState v m, SqlWhere v, KontraException e, FromSQL a, FromSQL b, FromSQL c) => (a -> b -> c -> e, SQL, SQL, SQL) -> SQL -> m ()
sqlWhereEVVV (exc, vsql1, vsql2, vsql3) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (SqlWhyNot True exc2 [vsql1, vsql2, vsql3])))
  where
    exc2 (v1, v2, v3) = exc v1 v2 v3

sqlWhereEVVVV :: (MonadState v m, SqlWhere v, KontraException e, FromSQL a, FromSQL b, FromSQL c, FromSQL d) => (a -> b -> c -> d -> e, SQL, SQL, SQL, SQL) -> SQL -> m ()
sqlWhereEVVVV (exc, vsql1, vsql2, vsql3, vsql4) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (SqlWhyNot True exc2 [vsql1, vsql2, vsql3, vsql4])))
  where
    exc2 (v1, v2, v3, v4) = exc v1 v2 v3 v4

sqlWhereEq :: (MonadState v m, SqlWhere v, Show a, ToSQL a) => SQL -> a -> m ()
sqlWhereEq name value = sqlWhere $ name <+> "=" <?> value

sqlWhereEqE :: (MonadState v m, SqlWhere v, KontraException e, Show a, FromSQL a, ToSQL a)
            => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereEqE exc name value = sqlWhereEV (exc value, name) $ name <+> "=" <?> value

sqlWhereEqSql :: (MonadState v m, SqlWhere v, Sqlable sql) => SQL -> sql -> m ()
sqlWhereEqSql name1 name2 = sqlWhere $ name1 <+> "=" <+> toSQLCommand name2

sqlWhereNotEq :: (MonadState v m, SqlWhere v, Show a, ToSQL a) => SQL -> a -> m ()
sqlWhereNotEq name value = sqlWhere $ name <+> "<>" <?> value

sqlWhereNotEqE :: (MonadState v m, SqlWhere v, KontraException e, Show a, ToSQL a, FromSQL a)
               => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereNotEqE exc name value = sqlWhereEV (exc value, name) $ name <+> "<>" <?> value

sqlWhereLike :: (MonadState v m, SqlWhere v, Show a, ToSQL a) => SQL -> a -> m ()
sqlWhereLike name value = sqlWhere $ name <+> "LIKE" <?> value

sqlWhereLikeE :: (MonadState v m, SqlWhere v, KontraException e, Show a, ToSQL a, FromSQL a)
              => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereLikeE exc name value = sqlWhereEV (exc value, name) $ name <+> "LIKE" <?> value

sqlWhereILike :: (MonadState v m, SqlWhere v, Show a, ToSQL a) => SQL -> a -> m ()
sqlWhereILike name value = sqlWhere  $ name <+> "ILIKE" <?> value

sqlWhereILikeE :: (MonadState v m, SqlWhere v, KontraException e, Show a, ToSQL a, FromSQL a)
               => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereILikeE exc name value = sqlWhereEV (exc value, name) $ name <+> "ILIKE" <?> value

sqlWhereIn :: (MonadState v m, SqlWhere v, Show a, ToSQL a) => SQL -> [a] -> m ()
sqlWhereIn _name [] = sqlWhere "FALSE"
sqlWhereIn name [value] = sqlWhereEq name value
sqlWhereIn name values = do
  -- Unpack the array to give query optimizer more options.
  sqlWhere $ name <+> "IN (SELECT UNNEST(" <?> Array1 values <+> "))"

sqlWhereInSql :: (MonadState v m, Sqlable a, SqlWhere v) => SQL -> a -> m ()
sqlWhereInSql name sql = sqlWhere $ name <+> "IN" <+> parenthesize (toSQLCommand sql)

sqlWhereInE :: (MonadState v m, SqlWhere v, KontraException e, Show a, ToSQL a, FromSQL a)
            => ([a] -> a -> e) -> SQL -> [a] -> m ()
sqlWhereInE exc name [] = sqlWhereEV (exc [], name) "FALSE"
sqlWhereInE exc name [value] = sqlWhereEqE (exc . (\x -> [x])) name value
sqlWhereInE exc name values =
  sqlWhereEV (exc values, name) $ name <+> "IN (SELECT UNNEST(" <?> Array1 values <+> "))"

sqlWhereNotIn :: (MonadState v m, SqlWhere v, Show a, ToSQL a) => SQL -> [a] -> m ()
sqlWhereNotIn _name [] = sqlWhere "TRUE"
sqlWhereNotIn name [value] = sqlWhereNotEq name value
sqlWhereNotIn name values = sqlWhere $ name <+> "NOT IN (SELECT UNNEST(" <?> Array1 values <+> "))"

sqlWhereNotInSql :: (MonadState v m, Sqlable a, SqlWhere v) => SQL -> a -> m ()
sqlWhereNotInSql name sql = sqlWhere $ name <+> "NOT IN" <+> parenthesize (toSQLCommand sql)

sqlWhereNotInE :: (MonadState v m, SqlWhere v, KontraException e, Show a, ToSQL a, FromSQL a)
               => ([a] -> a -> e) -> SQL -> [a] -> m ()
sqlWhereNotInE exc name [] = sqlWhereEV (exc [], name) "TRUE"
sqlWhereNotInE exc name [value] = sqlWhereNotEqE (exc . (\x -> [x])) name value
sqlWhereNotInE exc name values =
  sqlWhereEV (exc values, name) $ name <+> "NOT IN (SELECT UNNEST(" <?> Array1 values <+> "))"

sqlWhereExists :: (MonadState v m, SqlWhere v) => SqlSelect -> m ()
sqlWhereExists sql = do
  modify (\cmd -> sqlWhere1 cmd (SqlExistsCondition sql))

sqlWhereNotExists :: (MonadState v m, SqlWhere v) => SqlSelect -> m ()
sqlWhereNotExists sqlSelectD = do
  sqlWhere ("NOT EXISTS (" <+> toSQLCommand (sqlSelectD { sqlSelectResult = ["TRUE"] }) <+> ")")

sqlWhereIsNULL :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhereIsNULL col = sqlWhere $ col <+> "IS NULL"

sqlWhereIsNotNULL :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhereIsNotNULL col = sqlWhere $ col <+> "IS NOT NULL"

sqlWhereIsNULLE :: (MonadState v m, SqlWhere v, KontraException e, Show a, FromSQL a)
                => (a -> e) -> SQL -> m ()
sqlWhereIsNULLE exc col = sqlWhereEV (exc, col) $ col <+> "IS NULL"

sqlWhereAny :: (MonadState v m, SqlWhere v) => [State SqlAll ()] -> m ()
sqlWhereAny [] = sqlWhere "FALSE"
sqlWhereAny l = sqlWhere $ "(" <+> smintercalate "OR" (map (parenthesize . toSQLCommand . flip execState (SqlAll [])) l) <+> ")"

class SqlFrom a where
  sqlFrom1 :: a -> SQL -> a

instance SqlFrom SqlSelect where
  sqlFrom1 cmd sql = cmd { sqlSelectFrom = sqlSelectFrom cmd <+> sql }

instance SqlFrom SqlInsertSelect where
  sqlFrom1 cmd sql = cmd { sqlInsertSelectFrom = sqlInsertSelectFrom cmd <+> sql }

instance SqlFrom SqlUpdate where
  sqlFrom1 cmd sql = cmd { sqlUpdateFrom = sqlUpdateFrom cmd <+> sql }

instance SqlFrom SqlDelete where
  sqlFrom1 cmd sql = cmd { sqlDeleteUsing = sqlDeleteUsing cmd <+> sql }

sqlFrom :: (MonadState v m, SqlFrom v) => SQL -> m ()
sqlFrom sql = modify (\cmd -> sqlFrom1 cmd sql)

sqlJoin :: (MonadState v m, SqlFrom v) => SQL -> m ()
sqlJoin table = sqlFrom (", " <+> table)

sqlJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlJoinOn table condition = sqlFrom (" JOIN " <+>
                                     table <+>
                                     " ON " <+>
                                     condition)

sqlLeftJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlLeftJoinOn table condition = sqlFrom (" LEFT JOIN " <+>
                                         table <+>
                                         " ON " <+>
                                         condition)

sqlRightJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlRightJoinOn table condition = sqlFrom (" RIGHT JOIN " <+>
                                          table <+>
                                          " ON " <+>
                                          condition)

sqlFullJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlFullJoinOn table condition = sqlFrom (" FULL JOIN " <+>
                                         table <+>
                                         " ON " <+>
                                         condition)

class SqlSet a where
  sqlSet1 :: a -> SQL -> SQL -> a

instance SqlSet SqlUpdate where
  sqlSet1 cmd name v = cmd { sqlUpdateSet = sqlUpdateSet cmd ++ [(name, v)] }

instance SqlSet SqlInsert where
  sqlSet1 cmd name v = cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, Single v)] }

instance SqlSet SqlInsertSelect where
  sqlSet1 cmd name v = cmd { sqlInsertSelectSet = sqlInsertSelectSet cmd ++ [(name, v)] }

sqlSetCmd :: (MonadState v m, SqlSet v) => SQL -> SQL -> m ()
sqlSetCmd name sql = modify (\cmd -> sqlSet1 cmd name sql)

sqlSetCmdList :: (MonadState SqlInsert m) => SQL -> [SQL] -> m ()
sqlSetCmdList name as = modify (\cmd -> cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, Many as)] })

sqlSet :: (MonadState v m, SqlSet v, Show a, ToSQL a) => SQL -> a -> m ()
sqlSet name a = sqlSetCmd name (sqlParam a)

sqlSetInc :: (MonadState v m, SqlSet v) => SQL -> m ()
sqlSetInc name = sqlSetCmd name $ name <+> "+ 1"

sqlSetList :: (MonadState SqlInsert m, Show a, ToSQL a) => SQL -> [a] -> m ()
sqlSetList name as = sqlSetCmdList name (map sqlParam as)

sqlSetListWithDefaults :: (MonadState SqlInsert m, Show a, ToSQL a) => SQL -> [Maybe a] -> m ()
sqlSetListWithDefaults name as = sqlSetCmdList name (map (maybe "DEFAULT" sqlParam) as)

sqlCopyColumn :: (MonadState v m, SqlSet v) => SQL -> m ()
sqlCopyColumn column = sqlSetCmd column column

class SqlResult a where
  sqlResult1 :: a -> SQL -> a

instance SqlResult SqlSelect where
  sqlResult1 cmd sql = cmd { sqlSelectResult = sqlSelectResult cmd ++ [sql] }

instance SqlResult SqlInsert where
  sqlResult1 cmd sql = cmd { sqlInsertResult = sqlInsertResult cmd ++ [sql] }

instance SqlResult SqlInsertSelect where
  sqlResult1 cmd sql = cmd { sqlInsertSelectResult = sqlInsertSelectResult cmd ++ [sql] }

instance SqlResult SqlUpdate where
  sqlResult1 cmd sql = cmd { sqlUpdateResult = sqlUpdateResult cmd ++ [sql] }



sqlResult :: (MonadState v m, SqlResult v) => SQL -> m ()
sqlResult sql = modify (\cmd -> sqlResult1 cmd sql)

class SqlOrderBy a where
  sqlOrderBy1 :: a -> SQL -> a

instance SqlOrderBy SqlSelect where
  sqlOrderBy1 cmd sql = cmd { sqlSelectOrderBy = sqlSelectOrderBy cmd ++ [sql] }

instance SqlOrderBy SqlInsertSelect where
  sqlOrderBy1 cmd sql = cmd { sqlInsertSelectOrderBy = sqlInsertSelectOrderBy cmd ++ [sql] }


sqlOrderBy :: (MonadState v m, SqlOrderBy v) => SQL -> m ()
sqlOrderBy sql = modify (\cmd -> sqlOrderBy1 cmd sql)

class SqlGroupByHaving a where
  sqlGroupBy1 :: a -> SQL -> a
  sqlHaving1 :: a -> SQL -> a

instance SqlGroupByHaving SqlSelect where
  sqlGroupBy1 cmd sql = cmd { sqlSelectGroupBy = sqlSelectGroupBy cmd ++ [sql] }
  sqlHaving1 cmd sql = cmd { sqlSelectHaving = sqlSelectHaving cmd ++ [sql] }

instance SqlGroupByHaving SqlInsertSelect where
  sqlGroupBy1 cmd sql = cmd { sqlInsertSelectGroupBy = sqlInsertSelectGroupBy cmd ++ [sql] }
  sqlHaving1 cmd sql = cmd { sqlInsertSelectHaving = sqlInsertSelectHaving cmd ++ [sql] }

sqlGroupBy :: (MonadState v m, SqlGroupByHaving v) => SQL -> m ()
sqlGroupBy sql = modify (\cmd -> sqlGroupBy1 cmd sql)

sqlHaving :: (MonadState v m, SqlGroupByHaving v) => SQL -> m ()
sqlHaving sql = modify (\cmd -> sqlHaving1 cmd sql)


class SqlOffsetLimit a where
  sqlOffset1 :: a -> Integer -> a
  sqlLimit1 :: a -> Integer -> a

instance SqlOffsetLimit SqlSelect where
  sqlOffset1 cmd num = cmd { sqlSelectOffset = num }
  sqlLimit1 cmd num = cmd { sqlSelectLimit = num }

instance SqlOffsetLimit SqlInsertSelect where
  sqlOffset1 cmd num = cmd { sqlInsertSelectOffset = num }
  sqlLimit1 cmd num = cmd { sqlInsertSelectLimit = num }

sqlOffset :: (MonadState v m, SqlOffsetLimit v, Integral int) => int -> m ()
sqlOffset val = modify (\cmd -> sqlOffset1 cmd $ toInteger val)

sqlLimit :: (MonadState v m, SqlOffsetLimit v, Integral int) => int -> m ()
sqlLimit val = modify (\cmd -> sqlLimit1 cmd $ toInteger val)

class SqlDistinct a where
  sqlDistinct1 :: a -> a

instance SqlDistinct SqlSelect where
  sqlDistinct1 cmd = cmd { sqlSelectDistinct = True }

instance SqlDistinct SqlInsertSelect where
  sqlDistinct1 cmd = cmd { sqlInsertSelectDistinct = True }

sqlDistinct :: (MonadState v m, SqlDistinct v) => m ()
sqlDistinct = modify (\cmd -> sqlDistinct1 cmd)


class (SqlWhere a, Sqlable a) => SqlTurnIntoSelect a where
  sqlTurnIntoSelect :: a -> SqlSelect

instance SqlTurnIntoSelect SqlSelect where
  sqlTurnIntoSelect = id


-- | The 'sqlTurnIntoWhyNotSelect' turn a failed query into a
-- why-not-query that can explain why query altered zero rows or
-- returned zero results.
--
-- Lets consider an example of explanation:
--
-- > UPDATE t1
-- >    SET a = 1
-- >  WHERE cond1
-- >    AND cond2                       -- with value2
-- >    AND EXISTS (SELECT TRUE
-- >                  FROM t2
-- >                 WHERE cond3        -- with value3a and value3b
-- >                   AND EXISTS (SELECT TRUE
-- >                                 FROM t3
-- >                                WHERE cond4))
--
-- sqlTurnIntoWhyNotSelect will produce a SELECT of the form:
--
-- > SELECT
-- >   EXISTS (SELECT TRUE ... WHERE cond1),
-- >   EXISTS (SELECT TRUE ... WHERE cond1 AND cond2),
-- >   EXISTS (SELECT TRUE ... WHERE cond1 AND cond2 AND cond3),
-- >   EXISTS (SELECT TRUE ... WHERE cond1 AND cond2 AND cond3 AND cond4);
--
-- Now, after this statement is executed we see which of these
-- returned FALSE as the first one. This is the condition that failed
-- the whole query.
--
-- We can get more information at this point. If failed condition was
-- cond2, then value2 can be extracted by this statement:
--
-- > SELECT value2 ... WHERE cond1;
--
-- If failed condition was cond3, then statement executed can be:
--
-- > SELECT value3a, value3b ... WHERE cond1 AND cond2;
--
-- Rationale: EXISTS clauses should pinpoint which condX was the first
-- one to produce zero rows.  SELECT clauses after EXISTS should
-- explain why condX filtered out all rows.
--
-- 'DB.WhyNot.kWhyNot1' looks for first EXISTS clause that is FALSE
-- and then tries to construct Exception object with values that come
-- after. If values that comes after cannot be sensibly parsed
-- (usually they are NULL when a value is expected), this exception is
-- skipped and next one is tried.
--
-- If first EXISTS clause is TRUE but no other exception was properly
-- generated then DBExceptionCouldNotParseValues is thrown with pair
-- of typeRef of first exception that could not be parsed and with
-- list of SqlValues that it could not parse.
--
-- The 'DB.WhyNot.kRun1OrThrowWhyNot' throws first exception on the
-- list.
--
-- We have a theorem to use in this transformation:
--
-- > EXISTS (SELECT t1 WHERE cond1 AND EXISTS (SELECT t2 WHERE cond2))
--
-- is equivalent to
--
-- > EXISTS (SELECT t1, t2 WHERE cond1 AND cond2)
--
-- and it can be used recursivelly.
sqlTurnIntoWhyNotSelect :: (SqlTurnIntoSelect a) => a -> SqlSelect
sqlTurnIntoWhyNotSelect command =
    sqlSelect "" . sqlResult $ mconcat [
        "ARRAY["
      , mintercalate ", " $ map emitExists [0..(count-1)]
      , "]"
      ]
    where select = sqlTurnIntoSelect command
          count :: Int
          count = sum (map count' (sqlSelectWhere select))
          count' (SqlPlainCondition {}) = 1
          count' (SqlExistsCondition select') = sum (map count' (sqlSelectWhere select'))

          emitExists :: Int -> SQL
          emitExists current =
            case runState (run current select) 0 of
              (s, _) -> if null (sqlSelectWhere s)
                        then "TRUE"
                        else "EXISTS (" <> (toSQLCommand $ s { sqlSelectResult = [ "TRUE" ]}) <> ")"

          run :: (Monad m,MonadState Int m) => Int -> SqlSelect -> m SqlSelect
          run current select' = do
            new <- mapM (around current) (sqlSelectWhere select')
            return (select' { sqlSelectWhere = concat new })

          around :: (Monad m,MonadState Int m) => Int -> SqlCondition -> m [SqlCondition]
          around current cond@(SqlPlainCondition{}) = do
            index <- get
            modify (+1)
            if current >= index
              then return [cond]
              else return []
          around current (SqlExistsCondition subSelect) = do
            subSelect' <- run current subSelect
            return [SqlExistsCondition subSelect']


instance SqlTurnIntoSelect SqlUpdate where
  sqlTurnIntoSelect s = SqlSelect
                        { sqlSelectFrom    = sqlUpdateWhat s <>
                                             if isSqlEmpty (sqlUpdateFrom s)
                                             then ""
                                             else "," <+> sqlUpdateFrom s
                        , sqlSelectDistinct = False
                        , sqlSelectResult  = if null (sqlUpdateResult s)
                                             then ["TRUE"]
                                             else sqlUpdateResult s
                        , sqlSelectWhere   = sqlUpdateWhere s
                        , sqlSelectOrderBy = []
                        , sqlSelectGroupBy = []
                        , sqlSelectHaving  = []
                        , sqlSelectOffset  = 0
                        , sqlSelectLimit   = -1
                        , sqlSelectWith    = sqlUpdateWith s -- this is a bit dangerous because it can contain nested DELETE/UPDATE
                        }

instance SqlTurnIntoSelect SqlDelete where
  sqlTurnIntoSelect s = SqlSelect
                        { sqlSelectFrom    = sqlDeleteFrom s <>
                                             if isSqlEmpty (sqlDeleteUsing s)
                                             then ""
                                             else "," <+> sqlDeleteUsing s
                        , sqlSelectDistinct = False
                        , sqlSelectResult  = if null (sqlDeleteResult s)
                                             then ["TRUE"]
                                             else sqlDeleteResult s
                        , sqlSelectWhere   = sqlDeleteWhere s
                        , sqlSelectOrderBy = []
                        , sqlSelectGroupBy = []
                        , sqlSelectHaving  = []
                        , sqlSelectOffset  = 0
                        , sqlSelectLimit   = -1
                        , sqlSelectWith    = sqlDeleteWith s -- this is a bit dangerous because it can contain nested DELETE/UPDATE
                        }

instance SqlTurnIntoSelect SqlInsertSelect where
  sqlTurnIntoSelect s = SqlSelect
                        { sqlSelectFrom    = sqlInsertSelectFrom s
                        , sqlSelectDistinct = False
                        , sqlSelectResult  = sqlInsertSelectResult s
                        , sqlSelectWhere   = sqlInsertSelectWhere s
                        , sqlSelectOrderBy = sqlInsertSelectOrderBy s
                        , sqlSelectGroupBy = sqlInsertSelectGroupBy s
                        , sqlSelectHaving  = sqlInsertSelectHaving s
                        , sqlSelectOffset  = sqlInsertSelectOffset s
                        , sqlSelectLimit   = sqlInsertSelectLimit s
                        , sqlSelectWith    = sqlInsertSelectWith s -- this is a bit dangerous because it can contain nested DELETE/UPDATE
                        }
{-
data DBExceptionCouldNotParseValues = DBExceptionCouldNotParseValues TypeRep ConvertError [SqlValue]
  deriving (Eq, Show, Typeable)

instance KontraException DBExceptionCouldNotParseValues

instance JSON.ToJSValue DBExceptionCouldNotParseValues where
  toJSValue _ = JSON.runJSONGen $ do
                JSON.value "message" "DBExceptionCouldNotParseValues"
                JSON.value "http_status" (500::Int)
                -}
data DBBaseLineConditionIsFalse = DBBaseLineConditionIsFalse SQL
  deriving (Show, Typeable)

instance KontraException DBBaseLineConditionIsFalse

--
-- It it quite tempting to put the offending SQL as text in the JSON
-- that we produce.  This would aid debugging greatly, but could
-- possibly also reveal too much information to a potential attacker.
instance JSON.ToJSValue DBBaseLineConditionIsFalse where
  toJSValue _sql = JSON.runJSONGen $ do
                     JSON.value "message" ("DBBaseLineConditionIsFalse"::String)

{- Warning: use kWhyNot1 for now as kWhyNot does not work in expected way.

kWhyNot should return a list of rows, where each row is a list of
exceptions.  Right now we are not able to differentiate between rows
because we do not support a concept of a row identity. kWhyNot can
return rows in any order, returns empty rows for successful hits, does
not return a row if baseline conditions weren't met. This effectivelly
renders it useless.

kWhyNot will be resurrected when we get a row identity concept.

-}

{-
kWhyNot :: (SqlTurnIntoSelect s, MonadDB m) => s -> m [[SomeKontraException]]
kWhyNot cmd = do
  let newSelect = sqlTurnIntoWhyNotSelect cmd
  if null (sqlSelectResult newSelect)
     then return [[]]
     else do
       kRun_ newSelect
       kFold2 (decodeListOfExceptionsFromWhere (sqlGetWhereConditions cmd)) []
-}


-- | KontraException and SomeKontraException mimic Exception and
-- SomeException but we need our own class and data type to limit its
-- use to only those which describe semantic exceptions.
--
-- Our data types also feature conversion to json type so that
-- external representation is known in place where exception is
-- defined.
class (Show e, Typeable e, JSON.ToJSValue e) => KontraException e where
  toKontraException :: e -> SomeKontraException
  toKontraException = SomeKontraException
  fromKontraException :: SomeKontraException -> Maybe e
  fromKontraException (SomeKontraException e) = cast e

catchKontra :: (MonadBaseControl IO m, KontraException e) => m a -> (e -> m a) -> m a
catchKontra m f = m `E.catch` (\e -> case fromKontraException e of
                                         Just ke -> f ke
                                         Nothing -> throw e)


data SomeKontraException = forall e. (Show e, KontraException e) => SomeKontraException e
  deriving (Typeable)

instance Exception SomeKontraException where
  toException = SomeException
  fromException (SomeException e) = msum [ cast e
                                         , do
                                              DBException {dbeError = e'} <- cast e
                                              cast e'
                                         ]

deriving instance Show SomeKontraException

{-
instance Show SomeKontraException where
  show (SomeKontraException e) = show e
-}

-- | Function kWhyNot1 is a workhorse for explainable SQL
-- failures. SQL fails if it did not affect any rows or did no return
-- any rows.  When that happens kWhyNot1 should be called. kWhyNot1
-- returns a list of exceptions describing why a row could not be
-- returned or affected by a query.
--
-- If kWhyNot1 returns empty list of exception when none of EXISTS
-- clauses generated by sqlTurnIntoWhyNotSelect was FALSE. Should not
-- happen in real life, file a bug report if you see such a case.
--

data ExceptionMaker = forall row. FromRow row => ExceptionMaker (row -> SomeKontraException)

kWhyNot1Ex :: forall m s. (SqlTurnIntoSelect s, MonadDB m, MonadThrow m)
           => s -> m (Bool, SomeKontraException)
kWhyNot1Ex cmd = do
  let newSelect = sqlTurnIntoSelect cmd
      newWhyNotSelect = sqlTurnIntoWhyNotSelect newSelect
  let findFirstFalse :: Identity (Array1 Bool) -> Int
      findFirstFalse (Identity (Array1 row)) = fromMaybe 0 (findIndex (== False) row)
  runQuery_ (newWhyNotSelect { sqlSelectLimit = 1 })
  indexOfFirstFailedCondition <- fetchOne findFirstFalse

  let logics = enumerateWhyNotExceptions ((sqlSelectFrom newSelect),[]) (sqlGetWhereConditions newSelect)

  let condition = logics !! (indexOfFirstFailedCondition)

  case condition of
    (important, ExceptionMaker exception, _from, []) -> return (important, exception $ $unexpectedError "this argument should've been ignored")
    (important, ExceptionMaker exception, (from, conds), sqls) -> do
       let statement' = sqlSelect2 from $ do
             mapM_ sqlResult sqls
             sqlLimit 1
             sqlOffset 0
           statement = statement' { sqlSelectWhere = conds }
       --Log.debug $ "Explanation SQL:\n" ++ show statement
       runQuery_ statement
       result <- fetchOne exception
       return (important, result)

kWhyNot1 :: (SqlTurnIntoSelect s, MonadDB m, MonadThrow m)
         => s -> m SomeKontraException
kWhyNot1 cmd = snd `fmap` kWhyNot1Ex cmd

enumerateWhyNotExceptions :: (SQL, [SqlCondition])
                          -> [SqlCondition]
                          -> [( Bool
                              , ExceptionMaker
                              , (SQL, [SqlCondition])
                              , [SQL]
                              )]
enumerateWhyNotExceptions (from,condsUpTillNow) conds = concatMap worker (zip conds (inits conds))
  where
    worker (SqlPlainCondition _ (SqlWhyNot b f s), condsUpTillNow2) =
      [(b, ExceptionMaker (SomeKontraException . f), (from, condsUpTillNow ++ condsUpTillNow2), s)]
    worker (SqlExistsCondition s, condsUpTillNow2) =
      enumerateWhyNotExceptions (newFrom, condsUpTillNow ++ condsUpTillNow2)
                                  (sqlGetWhereConditions s)
      where
        newFrom = if isSqlEmpty from
                  then sqlSelectFrom s
                  else if isSqlEmpty (sqlSelectFrom s)
                       then from
                       else from <> ", " <> sqlSelectFrom s


kRunManyOrThrowWhyNot :: (SqlTurnIntoSelect s, MonadDB m, MonadThrow m)
                   => s -> m ()
kRunManyOrThrowWhyNot sqlable = do
  success <- runQuery $ toSQLCommand sqlable
  when (success == 0) $ do
    exception <- kWhyNot1 sqlable
    throwDB exception


kRun1OrThrowWhyNot :: (SqlTurnIntoSelect s, MonadDB m, MonadThrow m)
                   => s -> m ()
kRun1OrThrowWhyNot sqlable = do
  success <- runQuery01 $ toSQLCommand sqlable
  when (not success) $ do
    exception <- kWhyNot1 sqlable
    throwDB exception


kRun1OrThrowWhyNotAllowIgnore :: (SqlTurnIntoSelect s, MonadDB m, MonadThrow m)
                                => s -> m ()
kRun1OrThrowWhyNotAllowIgnore sqlable = do
  success <- runQuery01 $ toSQLCommand sqlable
  when (not success) $ do
    (important, exception) <- kWhyNot1Ex sqlable
    when (important) $
      throwDB exception

kRunAndFetch1OrThrowWhyNot :: (IsSQL s, FromRow row, MonadDB m, MonadThrow m, SqlTurnIntoSelect s)
                           => (row -> a) -> s -> m a
kRunAndFetch1OrThrowWhyNot decoder sqlcommand = do
  runQuery_ sqlcommand
  results <- fetchMany decoder
  case results of
    [] -> do
      exception <- kWhyNot1 sqlcommand
      throwDB exception
    [r] -> return r
    _ -> throwDB AffectedRowsMismatch {
      rowsExpected = [(1, 1)]
    , rowsDelivered = length results
    }
