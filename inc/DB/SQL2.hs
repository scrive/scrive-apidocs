{-# LANGUAGE ExtendedDefaultRules #-}
{- |

Module SQL2 offers some nice monadic function that build SQL commands on the fly. Some examples:


@

    kRun_ $ sqlInsert "documents" $ do
      sqlSet "title" title
      sqlSet "ctime" now
      sqlResult "id"

@

There is a possibility to do multiple inserts at once. Data given by
sqlSetList will be inserted multiple times, data given by sqlSet will
be multiplied as many times as needed to cover all inserted rows (it
is common to all rows). If you use multiple sqlSetList then lists will
be made equal in length by appending DEFAULT as fill element.

@

    kRun_ $ sqlInsert "documents" $ do
      sqlSet "ctime" now
      sqlSetList "title" [title1, title2, title3]
      sqlResult "id"

@

@

    kRun_ $ sqlSelect "documents" $ do
      sqlResult "id"
      sqlResult "title"
      sqlResult "mtime"
      sqlOrderBy "documents.mtime DESC"
      sqlOrderBy "documents.title"
      sqlGroupBy "documents.status"
      sqlJoinOn "users" "documents.user_id = users.id"
      sqlWhere $ SQL "documents.title ILIKE ?" [toSql pattern]

@

@

    kRun_ $ sqlDelete "mails" $ do
      sqlWhere "time + 14 > now()"

@

@

    kRun_ $ sqlUpdate "document_tags" $ do
      sqlSet "value" (123 :: Int)
      sqlWhere "name = 'abc'"

@

-}

module DB.SQL2
  ( sqlWhere
  , sqlWhereEq
  , sqlWhereNotEq
  , sqlWhereIn
  , sqlWhereNotIn
  , sqlWhereOr
  , sqlWhereExists
  , sqlWhereLike
  , sqlWhereILike
  , sqlWhereIsNULL
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
  , sqlResult
  , sqlOrderBy
  , sqlGroupBy
  , sqlHaving
  , sqlOffset
  , sqlLimit

  , sqlSelect
  , SqlSelect(..)
  , sqlInsert
  , SqlInsert(..)
  , sqlUpdate
  , SqlUpdate(..)
  , sqlDelete
  , SqlDelete(..)

  , SqlResult
  , SqlFrom
  , SqlWhere
  , SqlOrderBy
  , SqlGroupByHaving
  , SqlOffsetLimit
  )
  where

import DB.SQL
import DB.Functions ((.=))
import Control.Monad.State
import Data.List (transpose)
import Data.Monoid
import Database.HDBC
import Data.Convertible
import Data.Typeable

data Multiplicity a = Single a | Many [a]
  deriving (Eq, Ord, Show, Typeable)

data SqlSelect = SqlSelect
  { sqlSelectFrom    :: SQL
  , sqlSelectResult  :: [SQL]
  , sqlSelectWhere   :: [SQL]
  , sqlSelectOrderBy :: [SQL]
  , sqlSelectGroupBy :: [SQL]
  , sqlSelectHaving  :: [SQL]
  , sqlSelectOffset  :: Integer
  , sqlSelectLimit   :: Integer
  } deriving (Eq, Typeable)

data SqlUpdate = SqlUpdate
  { sqlUpdateWhat    :: RawSQL
  , sqlUpdateFrom    :: SQL
  , sqlUpdateWhere   :: [SQL]
  , sqlUpdateSet     :: [(RawSQL,SqlValue)]
  , sqlUpdateResult  :: [SQL]
  } deriving (Eq, Typeable)

data SqlInsert = SqlInsert
  { sqlInsertWhat    :: RawSQL
  , sqlInsertSet     :: [(RawSQL,Multiplicity SqlValue)]
  , sqlInsertResult  :: [SQL]
  } deriving (Eq, Typeable)

data SqlDelete = SqlDelete
  { sqlDeleteFrom    :: RawSQL
  , sqlDeleteWhere   :: [SQL]
  } deriving (Eq, Typeable)

instance Show SqlSelect where
  show = show . toSQLCommand

instance Show SqlInsert where
  show = show . toSQLCommand

instance Show SqlUpdate where
  show = show . toSQLCommand

instance Show SqlDelete where
  show = show . toSQLCommand

emitClause :: IsSQL sql => RawSQL -> sql -> SQL
emitClause name s = case toSQLCommand s of
  "" -> ""
  x -> "" <+> raw name <+> x

emitClausesSep :: RawSQL -> RawSQL -> [SQL] -> SQL
emitClausesSep _name _sep [] = SQL "" []
emitClausesSep name sep sqls = "" <+> raw name <+> intersperse sep (filter (/="") sqls)

instance IsSQL SqlSelect where
  toSQLCommand cmd =
    SQL "SELECT " [] `mappend`
        emitClausesSep "" ", " (sqlSelectResult cmd) `mappend`
        emitClause "FROM" (sqlSelectFrom cmd) `mappend`
        emitClausesSep "WHERE" "AND" (sqlSelectWhere cmd) `mappend`
        emitClausesSep "GROUP BY" ", " (sqlSelectGroupBy cmd) `mappend`
        emitClausesSep "HAVING" "AND" (sqlSelectHaving cmd) `mappend`
        emitClausesSep "ORDER BY" ", " (sqlSelectOrderBy cmd) `mappend`
        (if sqlSelectOffset cmd > 0
           then " OFFSET" <?> sqlSelectOffset cmd
           else "") `mappend`
        (if sqlSelectLimit cmd >= 0
           then " LIMIT" <?> sqlSelectLimit cmd
           else "")

instance IsSQL SqlInsert where
  toSQLCommand cmd =
    "INSERT INTO" <+> raw (sqlInsertWhat cmd) <+>
    parenthesize (sqlConcatComma (map (raw . fst) (sqlInsertSet cmd))) <+>
    emitClausesSep "VALUES" "," (map makeClause (transpose (map (makeLongEnough . snd) (sqlInsertSet cmd)))) `mappend`
    emitClausesSep "RETURNING" "," (sqlInsertResult cmd)
   where
      -- this is the longest list of values
      longest = maximum (1 : (map (lengthOfEither . snd) (sqlInsertSet cmd)))
      lengthOfEither (Single _) = 1
      lengthOfEither (Many x) = length x
      makeLongEnough (Single x) = take longest (repeat (sqlParam x))
      makeLongEnough (Many x) = take longest (map sqlParam x ++ repeat "DEFAULT")
      makeClause = parenthesize . sqlConcatComma

instance IsSQL SqlUpdate where
  toSQLCommand cmd =
    "UPDATE" <+> raw (sqlUpdateWhat cmd) <+> "SET" <+>
    sqlConcatComma (map (uncurry (.=)) (sqlUpdateSet cmd)) `mappend`
    emitClause "FROM" (sqlUpdateFrom cmd) `mappend`
    emitClausesSep "WHERE" "AND" (sqlUpdateWhere cmd) `mappend`
    emitClausesSep "RETURNING" "," (sqlUpdateResult cmd)

instance IsSQL SqlDelete where
  toSQLCommand cmd =
    "DELETE FROM" <+> raw (sqlDeleteFrom cmd) `mappend`
        emitClausesSep "WHERE" "AND" (sqlDeleteWhere cmd)


sqlSelect :: RawSQL -> State SqlSelect () -> SqlSelect
sqlSelect table refine =
  execState refine (SqlSelect (SQL table []) [] [] [] [] [] 0 (-1))

sqlInsert :: RawSQL -> State SqlInsert () -> SqlInsert
sqlInsert table refine =
  execState refine (SqlInsert table mempty [])

sqlUpdate :: RawSQL -> State SqlUpdate () -> SqlUpdate
sqlUpdate table refine =
  execState refine (SqlUpdate table mempty [] [] [])

sqlDelete :: RawSQL -> State SqlDelete () -> SqlDelete
sqlDelete table refine =
  execState refine (SqlDelete table [])

class SqlWhere a where
  sqlWhere1 :: a -> SQL -> a

instance SqlWhere SqlSelect where
  sqlWhere1 cmd sql = cmd { sqlSelectWhere = sqlSelectWhere cmd ++ [sql] }

instance SqlWhere SqlUpdate where
  sqlWhere1 cmd sql = cmd { sqlUpdateWhere = sqlUpdateWhere cmd ++ [sql] }

instance SqlWhere SqlDelete where
  sqlWhere1 cmd sql = cmd { sqlDeleteWhere = sqlDeleteWhere cmd ++ [sql] }

sqlWhere :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhere sql = modify (\cmd -> sqlWhere1 cmd sql)

sqlWhereOr :: (MonadState v m, SqlWhere v) => [SQL] -> m ()
sqlWhereOr [] = sqlWhere (SQL "FALSE" [])
sqlWhereOr [sql] = sqlWhere sql
sqlWhereOr sqls = sqlWhere $ parenthesize $ sqlConcatOR sqls

sqlWhereEq :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereEq name value = sqlWhere $ name <+> "=" <?> value

sqlWhereNotEq :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereNotEq name value = sqlWhere $ name <+> "<>" <?> value

sqlWhereLike :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereLike name value = sqlWhere $ name <+> "LIKE" <?> value

sqlWhereILike :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> a -> m ()
sqlWhereILike name value = sqlWhere  $ name <+> "ILIKE" <?> value

sqlWhereIn :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> [a] -> m ()
sqlWhereIn _name [] = sqlWhere (SQL "FALSE" [])
sqlWhereIn name [value] = sqlWhereEq name value
sqlWhereIn name values =
  sqlWhere $ name <+> "IN" <+> parenthesize (sqlConcatComma (map sqlParam values))

sqlWhereNotIn :: (MonadState v m, SqlWhere v, Convertible a SqlValue) => SQL -> [a] -> m ()
sqlWhereNotIn _name [] = sqlWhere "TRUE"
sqlWhereNotIn name [value] = sqlWhereNotEq name value
sqlWhereNotIn name values = sqlWhere $ name <+> "NOT IN" <+> parenthesize (sqlConcatComma (map sqlParam values))

sqlWhereExists :: (MonadState v m, SqlWhere v) => SqlSelect -> m ()
sqlWhereExists sqlSelectD = do
  sqlWhere (SQL "EXISTS (" [] `mappend` toSQLCommand (sqlSelectD { sqlSelectResult = [SQL "TRUE" []] }) `mappend` SQL ")" [])
  
sqlWhereIsNULL :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhereIsNULL col = sqlWhere $ col <+> "IS NULL"

class SqlFrom a where
  sqlFrom1 :: a -> SQL -> a

instance SqlFrom SqlSelect where
  sqlFrom1 cmd sql = cmd { sqlSelectFrom = sqlSelectFrom cmd `mappend` sql }

instance SqlFrom SqlUpdate where
  sqlFrom1 cmd sql = cmd { sqlUpdateFrom = sqlUpdateFrom cmd `mappend` sql }

sqlFrom :: (MonadState v m, SqlFrom v) => SQL -> m ()
sqlFrom sql = modify (\cmd -> sqlFrom1 cmd sql)

sqlJoin :: (MonadState v m, SqlFrom v) => SQL -> m ()
sqlJoin table = sqlFrom (SQL ", " [] `mappend` table)

sqlJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlJoinOn table condition = sqlFrom (SQL " JOIN " [] `mappend`
                                     table `mappend`
                                     SQL " ON " [] `mappend`
                                     condition)

sqlLeftJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlLeftJoinOn table condition = sqlFrom (SQL " LEFT JOIN " [] `mappend`
                                         table `mappend`
                                         SQL " ON " [] `mappend`
                                         condition)

sqlRightJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlRightJoinOn table condition = sqlFrom (SQL " RIGHT JOIN " [] `mappend`
                                          table `mappend`
                                          SQL " ON " [] `mappend`
                                          condition)

sqlFullJoinOn :: (MonadState v m, SqlFrom v) => SQL -> SQL -> m ()
sqlFullJoinOn table condition = sqlFrom (SQL " FULL JOIN " [] `mappend`
                                         table `mappend`
                                         SQL " ON " [] `mappend`
                                         condition)

class SqlSet a where
  sqlSet1 :: a -> RawSQL -> SqlValue -> a

instance SqlSet SqlUpdate where
  sqlSet1 cmd name v = cmd { sqlUpdateSet = sqlUpdateSet cmd ++ [(name, v)] }

instance SqlSet SqlInsert where
  sqlSet1 cmd name v = cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, Single v)] }


sqlSetCmd :: (MonadState v m, SqlSet v) => RawSQL -> SqlValue -> m ()
sqlSetCmd name sql = modify (\cmd -> sqlSet1 cmd name sql)

sqlSetCmdList :: (MonadState SqlInsert m) => RawSQL -> [SqlValue] -> m ()
sqlSetCmdList name as = modify (\cmd -> cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, Many as)] })

sqlSet :: (MonadState v m, SqlSet v, Convertible a SqlValue) => RawSQL -> a -> m ()
sqlSet name a = sqlSetCmd name (toSql a)

sqlSetList :: (MonadState SqlInsert m, Convertible a SqlValue) => RawSQL -> [a] -> m ()
sqlSetList name as = sqlSetCmdList name (map toSql as)

class SqlResult a where
  sqlResult1 :: a -> SQL -> a

instance SqlResult SqlSelect where
  sqlResult1 cmd sql = cmd { sqlSelectResult = sqlSelectResult cmd ++ [sql] }

instance SqlResult SqlInsert where
  sqlResult1 cmd sql = cmd { sqlInsertResult = sqlInsertResult cmd ++ [sql] }

instance SqlResult SqlUpdate where
  sqlResult1 cmd sql = cmd { sqlUpdateResult = sqlUpdateResult cmd ++ [sql] }



sqlResult :: (MonadState v m, SqlResult v) => SQL -> m ()
sqlResult sql = modify (\cmd -> sqlResult1 cmd sql)

class SqlOrderBy a where
  sqlOrderBy1 :: a -> SQL -> a

instance SqlOrderBy SqlSelect where
  sqlOrderBy1 cmd sql = cmd { sqlSelectOrderBy = sqlSelectOrderBy cmd ++ [sql] }



sqlOrderBy :: (MonadState v m, SqlOrderBy v) => SQL -> m ()
sqlOrderBy sql = modify (\cmd -> sqlOrderBy1 cmd sql)

class SqlGroupByHaving a where
  sqlGroupBy1 :: a -> SQL -> a
  sqlHaving1 :: a -> SQL -> a

instance SqlGroupByHaving SqlSelect where
  sqlGroupBy1 cmd sql = cmd { sqlSelectGroupBy = sqlSelectGroupBy cmd ++ [sql] }
  sqlHaving1 cmd sql = cmd { sqlSelectHaving = sqlSelectHaving cmd ++ [sql] }


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


sqlOffset :: (MonadState v m, SqlOffsetLimit v) => Integer -> m ()
sqlOffset val = modify (\cmd -> sqlOffset1 cmd val)

sqlLimit :: (MonadState v m, SqlOffsetLimit v) => Integer -> m ()
sqlLimit val = modify (\cmd -> sqlLimit1 cmd val)
