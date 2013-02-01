{-# LANGUAGE ExtendedDefaultRules, ExistentialQuantification #-}
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
>   sqlWhere "time + 14 > now()"

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

module DB.SQL2
  ( sqlWhere
  , sqlWhereE
  , sqlWhereEV
  , sqlWhereEVV
  , sqlWhereEVVV
  , sqlWhereEq
  , sqlWhereEqE
  , sqlWhereNotEq
  , sqlWhereNotEqE
  , sqlWhereIn
  , sqlWhereInE
  , sqlWhereNotIn
  , sqlWhereNotInE
  , sqlWhereExists
  , sqlWhereNotExists
  , sqlWhereLike
  , sqlWhereLikeE
  , sqlWhereILike
  , sqlWhereILikeE
  , sqlWhereIsNULL
  , sqlWhereIsNULLE


  , sqlFrom
  , sqlJoin
  , sqlJoinOn
  , sqlLeftJoinOn
  , sqlRightJoinOn
  , sqlFullJoinOn
  , sqlSet
  , sqlSetList
  , sqlSetCmd
  , sqlSetCmdList
  , sqlCopyColumn
  , sqlResult
  , sqlOrderBy
  , sqlGroupBy
  , sqlHaving
  , sqlOffset
  , sqlLimit
  , sqlWith

  , SqlTurnIntoSelect
  , sqlTurnIntoSelect
  , sqlTurnIntoWhyNotSelect

  , sqlSelect
  , SqlSelect(..)
  , sqlInsert
  , SqlInsert(..)
  , sqlInsertSelect
  , SqlInsertSelect(..)
  , sqlUpdate
  , SqlUpdate(..)
  , sqlDelete
  , SqlDelete(..)

  , SqlAny(..)
  , sqlWhereAny

  , SqlAll(..)
  , sqlWhereAll

  , SqlResult
  , SqlFrom
  , SqlWhere
  , SqlOrderBy
  , SqlGroupByHaving
  , SqlOffsetLimit


  , SqlCondition(..)
  , sqlGetWhereConditions

  , SqlWhyNot(..)

  , kWhyNot1
  , DBExceptionCouldNotParseValues(..)
  , kRun1OrThrowWhyNot
  , kRunAndFetch1OrThrowWhyNot

  , KontraException(..)
  , SomeKontraException(..)
  , DBBaseLineConditionIsFalse(..)
  )
  where

import DB.SQL
import DB.Core
import DB.Functions hiding (sql)
import DB.Env
import DB.Fetcher
import DB.Exception
import Control.Monad.State
import Control.Monad.Base
import Data.List (transpose)
import Data.Monoid
import Database.HDBC hiding (run)
import Data.Convertible
import Data.Typeable
import Control.Exception.Lifted
import Control.Applicative
import Data.Maybe
import qualified Text.JSON.Gen as JSON

data Multiplicity a = Single a | Many [a]
  deriving (Eq, Ord, Show, Typeable)


data SqlCondition = SqlPlainCondition SQL (Maybe SqlWhyNot)
                  | SqlExistsCondition SqlSelect
                    deriving (Typeable, Show)

data SqlWhyNot = forall e. (KontraException e) => SqlWhyNot ([SqlValue] -> Either ConvertError e) [SQL]

instance Eq SqlCondition where
  (SqlPlainCondition a _) == (SqlPlainCondition b _) = a == b
  (SqlExistsCondition a) == (SqlExistsCondition b) = a == b
  _ == _ = False

instance Show SqlWhyNot where
  show (SqlWhyNot exc expr) = "SqlWhyNot " ++ show (typeOf (fromRight (exc undefined))) ++ " " ++ show expr
    where fromRight ~(Right x) = x

instance IsSQL SqlCondition where
  toSQLCommand (SqlPlainCondition a _) = a
  toSQLCommand (SqlExistsCondition a) = "EXISTS (" <> toSQLCommand (a { sqlSelectResult = [SQL "TRUE" []] }) <> ")"

data SqlSelect = SqlSelect
  { sqlSelectFrom    :: SQL
  , sqlSelectResult  :: [SQL]
  , sqlSelectWhere   :: [SqlCondition]
  , sqlSelectOrderBy :: [SQL]
  , sqlSelectGroupBy :: [SQL]
  , sqlSelectHaving  :: [SQL]
  , sqlSelectOffset  :: Integer
  , sqlSelectLimit   :: Integer
  , sqlSelectWith    :: [(RawSQL,SQL)]
  } deriving (Eq, Typeable)

data SqlUpdate = SqlUpdate
  { sqlUpdateWhat    :: RawSQL
  , sqlUpdateFrom    :: SQL
  , sqlUpdateWhere   :: [SqlCondition]
  , sqlUpdateSet     :: [(RawSQL,SQL)]
  , sqlUpdateResult  :: [SQL]
  , sqlUpdateWith    :: [(RawSQL,SQL)]
  } deriving (Eq, Typeable)

data SqlInsert = SqlInsert
  { sqlInsertWhat    :: RawSQL
  , sqlInsertSet     :: [(RawSQL,Multiplicity SQL)]
  , sqlInsertResult  :: [SQL]
  , sqlInsertWith    :: [(RawSQL,SQL)]
  } deriving (Eq, Typeable)

data SqlInsertSelect = SqlInsertSelect
  { sqlInsertSelectWhat    :: RawSQL
  , sqlInsertSelectSet     :: [(RawSQL,SQL)]
  , sqlInsertSelectResult  :: [SQL]
  , sqlInsertSelectFrom    :: SQL
  , sqlInsertSelectWhere   :: [SqlCondition]
  , sqlInsertSelectOrderBy :: [SQL]
  , sqlInsertSelectGroupBy :: [SQL]
  , sqlInsertSelectHaving  :: [SQL]
  , sqlInsertSelectOffset  :: Integer
  , sqlInsertSelectLimit   :: Integer
  , sqlInsertSelectWith    :: [(RawSQL,SQL)]
  } deriving (Eq, Typeable)

data SqlDelete = SqlDelete
  { sqlDeleteFrom    :: RawSQL
  , sqlDeleteUsing   :: SQL
  , sqlDeleteWhere   :: [SqlCondition]
  , sqlDeleteResult  :: [SQL]
  , sqlDeleteWith    :: [(RawSQL,SQL)]
  } deriving (Eq, Typeable)

data SqlAny = SqlAny
  { sqlAnyWhere :: [SqlCondition]
  }

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

instance Show SqlAny where
  show = show . toSQLCommand


emitClause :: IsSQL sql => RawSQL -> sql -> SQL
emitClause name s = case toSQLCommand s of
  "" -> ""
  x -> raw name <+> x

emitClausesSep :: RawSQL -> RawSQL -> [SQL] -> SQL
emitClausesSep _name _sep [] = SQL "" []
emitClausesSep name sep sqls = raw name <+> intersperse sep (filter (/="") sqls)

emitClausesSepComma :: RawSQL -> [SQL] -> SQL
emitClausesSepComma _name [] = SQL "" []
emitClausesSepComma name sqls = raw name <+> sqlConcatComma (filter (/="") sqls)

instance IsSQL SqlSelect where
  toSQLCommand cmd =
        emitClausesSepComma "WITH" (map (\(name,command) -> raw name <+> "AS" <+> parenthesize command) (sqlSelectWith cmd)) <+>
        "SELECT" <+> sqlConcatComma (sqlSelectResult cmd) <+>
        emitClause "FROM" (sqlSelectFrom cmd) <+>
        emitClausesSep "WHERE" "AND" (map toSQLCommand $ sqlSelectWhere cmd) <+>
        emitClausesSepComma "GROUP BY" (sqlSelectGroupBy cmd) <+>
        emitClausesSep "HAVING" "AND" (sqlSelectHaving cmd) <+>
        emitClausesSepComma "ORDER BY" (sqlSelectOrderBy cmd) <+>
        (if sqlSelectOffset cmd > 0
           then raw (unsafeFromString ("OFFSET " ++ show (sqlSelectOffset cmd)))
           else "") <+>
        (if sqlSelectLimit cmd >= 0
           then raw (unsafeFromString ("LIMIT " ++ show (sqlSelectLimit cmd)))
           else "")

instance IsSQL SqlInsert where
  toSQLCommand cmd =
    emitClausesSepComma "WITH" (map (\(name,command) -> raw name <+> "AS" <+> parenthesize command) (sqlInsertWith cmd)) <+>
    "INSERT INTO" <+> raw (sqlInsertWhat cmd) <+>
    parenthesize (sqlConcatComma (map (raw . fst) (sqlInsertSet cmd))) <+>
    emitClausesSep "VALUES" "," (map makeClause (transpose (map (makeLongEnough . snd) (sqlInsertSet cmd)))) <+>
    emitClausesSepComma "RETURNING" (sqlInsertResult cmd)
   where
      -- this is the longest list of values
      longest = maximum (1 : (map (lengthOfEither . snd) (sqlInsertSet cmd)))
      lengthOfEither (Single _) = 1
      lengthOfEither (Many x) = length x
      makeLongEnough (Single x) = take longest (repeat x)
      makeLongEnough (Many x) = take longest (x ++ repeat "DEFAULT")
      makeClause = parenthesize . sqlConcatComma

instance IsSQL SqlInsertSelect where
  toSQLCommand cmd =
    "INSERT INTO" <+> raw (sqlInsertSelectWhat cmd) <+>
    parenthesize (sqlConcatComma (map (raw . fst) (sqlInsertSelectSet cmd))) <+>
    parenthesize (toSQLCommand (SqlSelect { sqlSelectFrom    = sqlInsertSelectFrom cmd
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

instance IsSQL SqlUpdate where
  toSQLCommand cmd =
    emitClausesSepComma "WITH" (map (\(name,command) -> raw name <+> "AS" <+> parenthesize command) (sqlUpdateWith cmd)) <+>
    "UPDATE" <+> raw (sqlUpdateWhat cmd) <+> "SET" <+>
    sqlConcatComma (map (\(name,command) -> raw name <> "=" <> command) (sqlUpdateSet cmd)) <+>
    emitClause "FROM" (sqlUpdateFrom cmd) <+>
    emitClausesSep "WHERE" "AND" (map toSQLCommand $ sqlUpdateWhere cmd) <+>
    emitClausesSepComma "RETURNING" (sqlUpdateResult cmd)

instance IsSQL SqlDelete where
  toSQLCommand cmd =
    emitClausesSepComma "WITH" (map (\(name,command) -> raw name <+> "AS" <+> parenthesize command) (sqlDeleteWith cmd)) <+>
    "DELETE FROM" <+> raw (sqlDeleteFrom cmd) <+>
    emitClause "USING" (sqlDeleteUsing cmd) <+>
        emitClausesSep "WHERE" "AND" (map toSQLCommand $ sqlDeleteWhere cmd) <+>
    emitClausesSepComma "RETURNING" (sqlDeleteResult cmd)

instance IsSQL SqlAny where
  toSQLCommand cmd | null (sqlAnyWhere cmd) = "FALSE"
  toSQLCommand cmd =
    "(" <+> intersperse "OR" (map (parenthesize . toSQLCommand) (sqlAnyWhere cmd)) <+> ")"

instance IsSQL SqlAll where
  toSQLCommand cmd | null (sqlAllWhere cmd) = "TRUE"
  toSQLCommand cmd =
    "(" <+> intersperse "AND" (map (parenthesize . toSQLCommand) (sqlAllWhere cmd)) <+> ")"


sqlSelect :: RawSQL -> State SqlSelect () -> SqlSelect
sqlSelect table refine =
  execState refine (SqlSelect (SQL table []) [] [] [] [] [] 0 (-1) [])

sqlInsert :: RawSQL -> State SqlInsert () -> SqlInsert
sqlInsert table refine =
  execState refine (SqlInsert table mempty [] [])

sqlInsertSelect :: RawSQL -> SQL -> State SqlInsertSelect () -> SqlInsertSelect
sqlInsertSelect table from refine =
  execState refine (SqlInsertSelect
                    { sqlInsertSelectWhat    = table
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

sqlUpdate :: RawSQL -> State SqlUpdate () -> SqlUpdate
sqlUpdate table refine =
  execState refine (SqlUpdate table mempty [] [] [] [])

sqlDelete :: RawSQL -> State SqlDelete () -> SqlDelete
sqlDelete table refine =
  execState refine (SqlDelete  { sqlDeleteFrom   = table
                               , sqlDeleteUsing  = SQL "" []
                               , sqlDeleteWhere  = []
                               , sqlDeleteResult = []
                               , sqlDeleteWith   = []
                               })

class SqlWith a where
  sqlWith1 :: a -> RawSQL -> SQL -> a


instance SqlWith SqlSelect where
  sqlWith1 cmd name sql = cmd { sqlSelectWith = sqlSelectWith cmd ++ [(name,sql)] }

instance SqlWith SqlInsertSelect where
  sqlWith1 cmd name sql = cmd { sqlInsertSelectWith = sqlInsertSelectWith cmd ++ [(name,sql)] }

instance SqlWith SqlUpdate where
  sqlWith1 cmd name sql = cmd { sqlUpdateWith = sqlUpdateWith cmd ++ [(name,sql)] }

instance SqlWith SqlDelete where
  sqlWith1 cmd name sql = cmd { sqlDeleteWith = sqlDeleteWith cmd ++ [(name,sql)] }

sqlWith :: (MonadState v m, SqlWith v, IsSQL s) => RawSQL -> s -> m ()
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

instance SqlWhere SqlAny where
  sqlWhere1 cmd cond = cmd { sqlAnyWhere = sqlAnyWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlAnyWhere

instance SqlWhere SqlAll where
  sqlWhere1 cmd cond = cmd { sqlAllWhere = sqlAllWhere cmd ++ [cond] }
  sqlGetWhereConditions = sqlAllWhere

sqlWhere :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhere sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql Nothing))

sqlWhereE :: (MonadState v m, SqlWhere v, KontraException e) => e -> SQL -> m ()
sqlWhereE exc sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (Just (SqlWhyNot exc2 []))))
  where
    exc2 [] = return exc
    exc2 vs = error $ "sqlWhereE.exc2  should be given 0 values, got " ++ show vs

sqlWhereEV :: (MonadState v m, SqlWhere v, KontraException e, Convertible SqlValue a) => (a -> e,SQL) -> SQL -> m ()
sqlWhereEV (exc,vsql) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (Just (SqlWhyNot exc2 [vsql]))))
  where
    exc2 [v1] = exc <$> safeFromSql v1
    exc2 vs = error $ "sqlWhereEV.exc2  should be given 1 values, got " ++ show vs

sqlWhereEVV :: (MonadState v m, SqlWhere v, KontraException e, Convertible SqlValue a, Convertible SqlValue b) => (a -> b -> e,SQL,SQL) -> SQL -> m ()
sqlWhereEVV (exc,vsql1,vsql2) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (Just (SqlWhyNot exc2 [vsql1,vsql2]))))
  where
    exc2 [v1,v2] = exc <$> safeFromSql v1 <*> safeFromSql v2
    exc2 vs = error $ "sqlWhereEV.exc2  should be given 2 values, got " ++ show vs

sqlWhereEVVV :: (MonadState v m, SqlWhere v, KontraException e, Convertible SqlValue a, Convertible SqlValue b, Convertible SqlValue c) => (a -> b -> c -> e,SQL,SQL,SQL) -> SQL -> m ()
sqlWhereEVVV (exc,vsql1,vsql2,vsql3) sql = modify (\cmd -> sqlWhere1 cmd (SqlPlainCondition sql (Just (SqlWhyNot exc2 [vsql1, vsql2, vsql3]))))
  where
    exc2 [v1,v2,v3] = exc <$> safeFromSql v1 <*> safeFromSql v2 <*> safeFromSql v3
    exc2 vs = error $ "sqlWhereEV.exc2  should be given 3 values, got " ++ show vs

sqlWhereEq :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereEq name value = sqlWhere $ name <+> "=" <?> value

sqlWhereEqE :: (MonadState v m, SqlWhere v, KontraException e, Convertible a SqlValue, Convertible SqlValue a)
            => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereEqE exc name value = sqlWhereEV (exc value, name) $ name <+> "=" <?> value

sqlWhereNotEq :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereNotEq name value = sqlWhere $ name <+> "<>" <?> value

sqlWhereNotEqE :: (MonadState v m, SqlWhere v, KontraException e, Convertible a SqlValue, Convertible SqlValue a)
               => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereNotEqE exc name value = sqlWhereEV (exc value, name) $ name <+> "<>" <?> value

sqlWhereLike :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereLike name value = sqlWhere $ name <+> "LIKE" <?> value

sqlWhereLikeE :: (MonadState v m, SqlWhere v, KontraException e, Convertible a SqlValue, Convertible SqlValue a)
              => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereLikeE exc name value = sqlWhereEV (exc value, name) $ name <+> "LIKE" <?> value

sqlWhereILike :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereILike name value = sqlWhere  $ name <+> "ILIKE" <?> value

sqlWhereILikeE :: (MonadState v m, SqlWhere v, KontraException e, Convertible a SqlValue, Convertible SqlValue a)
               => (a -> a -> e) -> SQL -> a -> m ()
sqlWhereILikeE exc name value = sqlWhereEV (exc value, name) $ name <+> "ILIKE" <?> value

sqlWhereIn :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> [a] -> m ()
sqlWhereIn _name [] = sqlWhere (SQL "FALSE" [])
sqlWhereIn name [value] = sqlWhereEq name value
sqlWhereIn name values =
  sqlWhere $ name <+> "IN" <+> parenthesize (sqlConcatComma (map sqlParam values))

sqlWhereInE :: (MonadState v m, SqlWhere v, KontraException e, Convertible a SqlValue, Convertible SqlValue a)
            => ([a] -> a -> e) -> SQL -> [a] -> m ()
sqlWhereInE exc name [] = sqlWhereEV (exc [], name) (SQL "FALSE" [])
sqlWhereInE exc name [value] = sqlWhereEqE (exc . (\x -> [x])) name value
sqlWhereInE exc name values =
  sqlWhereEV (exc values, name) $ name <+> "IN" <+> parenthesize (sqlConcatComma (map sqlParam values))

sqlWhereNotIn :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> [a] -> m ()
sqlWhereNotIn _name [] = sqlWhere "TRUE"
sqlWhereNotIn name [value] = sqlWhereNotEq name value
sqlWhereNotIn name values = sqlWhere $ name <+> "NOT IN" <+> parenthesize (sqlConcatComma (map sqlParam values))

sqlWhereNotInE :: (MonadState v m, SqlWhere v, KontraException e, Convertible a SqlValue, Convertible SqlValue a)
               => ([a] -> a -> e) -> SQL -> [a] -> m ()
sqlWhereNotInE exc name [] = sqlWhereEV (exc [], name) "TRUE"
sqlWhereNotInE exc name [value] = sqlWhereNotEqE (exc . (\x -> [x])) name value
sqlWhereNotInE exc name values =
  sqlWhereEV (exc values, name) $ name <+> "NOT IN" <+> parenthesize (sqlConcatComma (map sqlParam values))

sqlWhereExists :: (MonadState v m, SqlWhere v) => SqlSelect -> m ()
sqlWhereExists sql = do
  modify (\cmd -> sqlWhere1 cmd (SqlExistsCondition sql))

sqlWhereNotExists :: (MonadState v m, SqlWhere v) => SqlSelect -> m ()
sqlWhereNotExists sqlSelectD = do
  sqlWhere (SQL "NOT EXISTS (" [] <+> toSQLCommand (sqlSelectD { sqlSelectResult = [SQL "TRUE" []] }) <+> SQL ")" [])

sqlWhereIsNULL :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhereIsNULL col = sqlWhere $ col <+> "IS NULL"

sqlWhereIsNULLE :: (MonadState v m, SqlWhere v, KontraException e, Convertible SqlValue a)
                => (a -> e) -> SQL -> m ()
sqlWhereIsNULLE exc col = sqlWhereEV (exc, col) $ col <+> "IS NULL"

sqlWhereAny :: (MonadState v m, SqlWhere v) => State SqlAny () -> m ()
sqlWhereAny = sqlWhere . toSQLCommand . flip execState (SqlAny [])

sqlWhereAll :: (MonadState v m, SqlWhere v) => State SqlAll () -> m ()
sqlWhereAll = sqlWhere . toSQLCommand . flip execState (SqlAll [])


class SqlFrom a where
  sqlFrom1 :: a -> SQL -> a

instance SqlFrom SqlSelect where
  sqlFrom1 cmd sql = cmd { sqlSelectFrom = sqlSelectFrom cmd <+> sql }

instance SqlFrom SqlInsertSelect where
  sqlFrom1 cmd sql = cmd { sqlInsertSelectFrom = sqlInsertSelectFrom cmd <+> sql }

instance SqlFrom SqlUpdate where
  sqlFrom1 cmd sql = cmd { sqlUpdateFrom = sqlUpdateFrom cmd <+> sql }

sqlFrom :: (MonadState v m, SqlFrom v) => SQL -> m ()
sqlFrom sql = modify (\cmd -> sqlFrom1 cmd sql)

sqlJoin :: (MonadState v m, SqlFrom v) => SQL -> m ()
sqlJoin table = sqlFrom (SQL ", " [] <+> table)

sqlJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlJoinOn table condition = sqlFrom (SQL " JOIN " [] <+>
                                     table <+>
                                     SQL " ON " [] <+>
                                     condition)

sqlLeftJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlLeftJoinOn table condition = sqlFrom (SQL " LEFT JOIN " [] <+>
                                         table <+>
                                         SQL " ON " [] <+>
                                         condition)

sqlRightJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlRightJoinOn table condition = sqlFrom (SQL " RIGHT JOIN " [] <+>
                                          table <+>
                                          SQL " ON " [] <+>
                                          condition)

sqlFullJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlFullJoinOn table condition = sqlFrom (SQL " FULL JOIN " [] <+>
                                         table <+>
                                         SQL " ON " [] <+>
                                         condition)

class SqlSet a where
  sqlSet1 :: a -> RawSQL -> SQL -> a

instance SqlSet SqlUpdate where
  sqlSet1 cmd name v = cmd { sqlUpdateSet = sqlUpdateSet cmd ++ [(name, v)] }

instance SqlSet SqlInsert where
  sqlSet1 cmd name v = cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, Single v)] }

instance SqlSet SqlInsertSelect where
  sqlSet1 cmd name v = cmd { sqlInsertSelectSet = sqlInsertSelectSet cmd ++ [(name, v)] }

sqlSetCmd :: (MonadState v m, SqlSet v) => RawSQL -> SQL -> m ()
sqlSetCmd name sql = modify (\cmd -> sqlSet1 cmd name sql)

sqlSetCmdList :: (MonadState SqlInsert m) => RawSQL -> [SQL] -> m ()
sqlSetCmdList name as = modify (\cmd -> cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, Many as)] })

sqlSet :: (MonadState v m, SqlSet v, Convertible a SqlValue) => RawSQL -> a -> m ()
sqlSet name a = sqlSetCmd name (sqlParam a)

sqlSetList :: (MonadState SqlInsert m, Convertible a SqlValue) => RawSQL -> [a] -> m ()
sqlSetList name as = sqlSetCmdList name (map sqlParam as)

sqlCopyColumn :: (MonadState v m, SqlSet v) => RawSQL -> m ()
sqlCopyColumn column = sqlSetCmd column (raw column)

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

sqlOffset :: (MonadState v m, SqlOffsetLimit v) => Integer -> m ()
sqlOffset val = modify (\cmd -> sqlOffset1 cmd val)

sqlLimit :: (MonadState v m, SqlOffsetLimit v) => Integer -> m ()
sqlLimit val = modify (\cmd -> sqlLimit1 cmd val)


class (SqlWhere a, IsSQL a) => SqlTurnIntoSelect a where
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
-- >  WHERE baselineA
-- >    AND cond1                       -- with value2
-- >    AND EXISTS (SELECT TRUE
-- >                  FROM t2
-- >                 WHERE baselineB
-- >                   AND cond3        -- with value4 and value5
-- >                   AND EXISTS (SELECT TRUE
-- >                                 FROM t3
-- >                                WHERE baselineC
-- >                                  AND cond6))
--
-- sqlTurnIntoWhyNotSelect will produce a SELECT of the form:
--
-- > SELECT
-- >   EXISTS (SELECT TRUE ... WHERE baseline),
-- >   EXISTS (SELECT TRUE ... WHERE baseline AND cond1),
-- >   (SELECT value2 ... WHERE baseline),
-- >   EXISTS (SELECT TRUE ... WHERE baseline AND cond1 AND cond3),
-- >   (SELECT value4 ... WHERE baseline AND cond1),
-- >   (SELECT value5 ... WHERE baseline AND cond1),
-- >   EXISTS (SELECT TRUE ... WHERE baseline AND cond1 AND cond3 AND cond6);
--
-- Where baseline = baselineA AND baselineB AND baselineC.
--
-- Rationale: EXISTS clauses should pinpoint which condX was the first
-- one to produce zero rows.  SELECT clauses after EXISTS should
-- explain why condX filtered out all rows.
--
-- Baseline conditions are treated as definitional: they have to be
-- there all the time because they represent relations between tables.
--
-- 'DB.WhyNot.kWhyNot1' looks for first EXISTS clause that is FALSE
-- and then tries to construct Exception object with values that come
-- after. If values that comes after cannot be sensibly parsed
-- (usually they are NULL when a value is expected), this exception is
-- skipped and next one is tried.
--
-- First EXISTS clause tests only baseline conditions and if is FALSE
-- then DBBaseLineConditionIsFalse is thrown. There is nothing more
-- that can be explained in this query.
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
sqlTurnIntoWhyNotSelect sel = sqlSelect "" $ do
                               mapM_ (sqlResult . pivotAroundCondition) [0..c]
    where sel2 = sqlTurnIntoSelect sel
          c = countExplainableConditions sel2
          maybeCommaAppend sql1 sql2 | sql2 == "" = sql1
          maybeCommaAppend sql1 sql2 = sql1 <> "," <> sql2
          -- c is index of condition that is currently in focus (starting with 1)
          -- that means this condition itselft will be removed and whole statement
          -- will be converted into EXISTS clause or value clause
          pivotAroundCondition cx =
            case runState (run cx sel2) (0,(Nothing :: Maybe (SQL,SQL))) of
              (s, (_,Nothing)) -> if null (sqlSelectWhere s)
                                  then
                                    -- we treat SELECT without WHERE as always having results
                                    "TRUE"
                                  else "EXISTS (" <> (toSQLCommand $ s { sqlSelectResult = [ "TRUE" ]}) <> ")"
              (s, (_,Just (f,e))) -> "(" <> (toSQLCommand $ s { sqlSelectFrom = maybeCommaAppend (sqlSelectFrom s) f
                                                              , sqlSelectResult = [ e ]
                                                              , sqlSelectLimit = 1
                                                              }) <> ")"
          run cx selx = do
            new <- mapM (around cx) (sqlSelectWhere selx)
            return (selx { sqlSelectWhere = concat new })

          around cx a@(SqlPlainCondition _ (Just (SqlWhyNot _ e))) = do
            idx <- fmap (+1) $ gets fst
            modify (\(cidx,b) -> (cidx+1+length e,b))
            case True of
              _ | cx > idx + length e -> do
                -- we passed past this particular condition and its explanations, treat it as mandatory one
                return [a]
              _ | cx == idx -> do
                return [a]
              _ | cx > idx -> do
                -- we are exactly at this point, that means we should
                -- emit explanation here condition goes away, but the
                -- explanation comes to life
                modify (\(cidx,_) -> (cidx,Just ("",e !! (cx- idx -1))))
                return []
              _ | otherwise -> do
                -- we haven't come to this part yet
                return []
          around _ a@(SqlPlainCondition _ Nothing) = do
            return [a]
          around cx (SqlExistsCondition subSelect) = do
            m <- get
            subSelect' <- run cx subSelect
            -- so we have run this subexists condition and there was a
            -- value of interest we need to concatenate where clauses,
            -- concatenate from caluses, use value as return statement
            k <- get
            case k of
              (s, Just (f,g)) | isNothing (snd m)-> do
                -- append FROM clauses using the theorem described above
                put (s, Just (maybeCommaAppend (sqlSelectFrom subSelect') f, g))
                return (sqlSelectWhere subSelect')
              _ -> do
                return [SqlExistsCondition subSelect']


countExplainableConditions :: SqlSelect -> Int
countExplainableConditions sel = execState (myRun sel) 0
  where
    myRun select = do
      mapM_ worker (sqlSelectWhere select)
    worker (SqlPlainCondition _ (Just (SqlWhyNot _ x))) = modify (\w -> w+1+length x)
    worker (SqlPlainCondition _ Nothing) = return ()
    worker (SqlExistsCondition select) = myRun select


instance SqlTurnIntoSelect SqlUpdate where
  sqlTurnIntoSelect s = SqlSelect
                        { sqlSelectFrom    = raw (sqlUpdateWhat s) <>
                                             if sqlUpdateFrom s == SQL "" []
                                             then ""
                                             else "," <+> sqlUpdateFrom s
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
                        { sqlSelectFrom    = raw (sqlDeleteFrom s) <>
                                             if sqlDeleteUsing s == SQL "" []
                                             then ""
                                             else "," <+> sqlDeleteUsing s
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
                        , sqlSelectResult  = sqlInsertSelectResult s
                        , sqlSelectWhere   = sqlInsertSelectWhere s
                        , sqlSelectOrderBy = sqlInsertSelectOrderBy s
                        , sqlSelectGroupBy = sqlInsertSelectGroupBy s
                        , sqlSelectHaving  = sqlInsertSelectHaving s
                        , sqlSelectOffset  = sqlInsertSelectOffset s
                        , sqlSelectLimit   = sqlInsertSelectLimit s
                        , sqlSelectWith    = sqlInsertSelectWith s -- this is a bit dangerous because it can contain nested DELETE/UPDATE
                        }

data DBExceptionCouldNotParseValues = DBExceptionCouldNotParseValues TypeRep ConvertError [SqlValue]
  deriving (Eq, Show, Typeable)

instance KontraException DBExceptionCouldNotParseValues

instance JSON.ToJSValue DBExceptionCouldNotParseValues where
  toJSValue _ = JSON.runJSONGen $ do
                JSON.value "message" "DBExceptionCouldNotParseValues"
                JSON.value "http_status" (500::Int)

data DBBaseLineConditionIsFalse = DBBaseLineConditionIsFalse SQL
  deriving (Eq, Show, Typeable)

instance KontraException DBBaseLineConditionIsFalse

instance JSON.ToJSValue DBBaseLineConditionIsFalse where
  toJSValue _ = JSON.runJSONGen $ do
                JSON.value "message" "DBBaseLineConditionIsFalse"
                JSON.value "http_status" (403::Int)

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
kWhyNot :: (SqlTurnIntoSelect s, MonadDB m) => s -> DBEnv m [[SomeKontraException]]
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


data SomeKontraException = forall e. (Show e, KontraException e) => SomeKontraException e
  deriving (Typeable)

instance Exception SomeKontraException

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
-- If kWhyNot1 returns empty list then it can be interpreted twofold:
-- either baseline conditions weren't met and therefore explainable
-- conditions could not be executed or in fact all explainable
-- conditions succeeded.
--
-- (In the second case your original query should have succedded
-- also. File a bug report if this is not the case.)
kWhyNot1 :: (SqlTurnIntoSelect s, MonadDB m) => s -> DBEnv m [SomeKontraException]
kWhyNot1 cmd = do
  let newSelect = sqlTurnIntoWhyNotSelect cmd
  case sqlSelectResult newSelect of
    [] -> do
      -- this should not actually happen due to DBBaseLineConditionIsFalse
      return []
    (h:_) -> do
       kRun_ (newSelect { sqlSelectLimit = 1 })
       result <- kFold2 (decodeListOfExceptionsFromWhere h (sqlGetWhereConditions cmd)) []

       case result of
         [] -> return []
         [e] -> return e
         _ -> error "Number of results in kWhyNot1 should be limited to 1, returned more"

enumerateWhyNotExceptions :: [SqlCondition] -> [SqlWhyNot]
enumerateWhyNotExceptions conds = concatMap worker conds
  where
    worker (SqlPlainCondition _ Nothing) = []
    worker (SqlPlainCondition _ (Just x)) = [x]
    worker (SqlExistsCondition s) = enumerateWhyNotExceptions (sqlGetWhereConditions s)

matchUpExceptionWithValues :: [SqlWhyNot] -> [SqlValue] -> [SomeKontraException]
matchUpExceptionWithValues [] [] = []
matchUpExceptionWithValues (SqlWhyNot e qs : es) (b : vs) =
  if b == SqlBool False
    then [exc] ++ matchUpExceptionWithValues es vs2
    else matchUpExceptionWithValues es vs2
  where
    vs1 = take (length qs) vs
    vs2 = drop (length qs) vs
    exc = case e vs1 of
            Right x -> toKontraException x
            Left l -> toKontraException (DBExceptionCouldNotParseValues (typeOf (fromRight (e vs1))) l vs1)
    fromRight ~(Right x) = x

matchUpExceptionWithValues (_ : _) [] = error "There were not enough values from SQL to fill in all needs for SqlWhyNotException in matchUpExceptionWithValues"
matchUpExceptionWithValues [] (_ : _) = error "There were not enough SqlWhyNotException's to use up all give SQL values in matchUpExceptionWithValues"

decodeListOfExceptionsFromWhere :: SQL -> [SqlCondition] -> [[SomeKontraException]] -> [SqlValue] -> Either SQLError [[SomeKontraException]]
decodeListOfExceptionsFromWhere fullquery conds excepts sqlvalues =
  return $ excepts ++
           [matchUpExceptionWithValues (SqlWhyNot (\_ -> return (DBBaseLineConditionIsFalse fullquery)) []
                                                    : enumerateWhyNotExceptions conds) sqlvalues]

kRun1OrThrowWhyNot :: (SqlTurnIntoSelect s, MonadDB m, MonadBase IO m)
                   => s -> DBEnv m ()
kRun1OrThrowWhyNot sqlcommand = do
  success <- kRun01 sqlcommand
  when (not success) $ do
    listOfExceptions <- kWhyNot1 sqlcommand

    case listOfExceptions of
      [] -> do
        -- This case should not really happen due to how we handle
        -- DBBaseLineConditionIsFalse in decodeListOfExceptionsFromWhere
        -- Just to be extra safe we put DBBaseLineConditionIsFalse here.
        throwIO $ toKontraException $ DBBaseLineConditionIsFalse (toSQLCommand sqlcommand)
      (ex:_) -> do
        -- Lets throw first exception on the list. It should be the
        -- most generic one.
        throwIO ex

kRunAndFetch1OrThrowWhyNot :: (SqlTurnIntoSelect s, MonadDB m, MonadBase IO m, Fetcher v [a])
                           => ([a] -> v) -> s -> DBEnv m a
kRunAndFetch1OrThrowWhyNot decoder sqlcommand = do
  kRun_ sqlcommand
  results <- kFold decoder []
  case results of
    [] -> do
      listOfExceptions <- kWhyNot1 sqlcommand

      case listOfExceptions of
        [] -> do
            -- This case should not really happen due to how we handle
            -- DBBaseLineConditionIsFalse in decodeListOfExceptionsFromWhere
            -- Just to be extra safe we put DBBaseLineConditionIsFalse here.
            throwIO $ toKontraException $ DBBaseLineConditionIsFalse (toSQLCommand sqlcommand)
        (ex:_) -> do
            -- Lets throw first exception on the list. It should be the
            -- most generic one.
            throwIO ex
    [r] -> return r
    _ -> liftIO $ throwIO TooManyObjects { originalQuery = mempty
                                         , tmoExpected = 1
                                         , tmoGiven = fromIntegral $ length results
                                         }
