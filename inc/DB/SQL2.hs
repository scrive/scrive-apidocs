
{- |

Module SQL2 offers some nice monadic function that build SQL commands on the fly. Some examples:


@

    kRun_ $ sqlInsert "documents" $ do
      sqlSet "title" title
      sqlSet "ctime" now
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
  , sqlSetCmd
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

import DB.SQL hiding(sql, sql')
import Control.Monad.State
import Data.Monoid
import Data.List
import Database.HDBC
import Data.Convertible
import Data.Typeable

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
  { sqlUpdateWhat    :: String
  , sqlUpdateFrom    :: SQL
  , sqlUpdateWhere   :: [SQL]
  , sqlUpdateSet     :: [(String,SQL)]
  , sqlUpdateResult  :: [SQL]
  } deriving (Eq, Typeable)

data SqlInsert = SqlInsert
  { sqlInsertWhat    :: String
  , sqlInsertSet     :: [(String,SQL)]
  , sqlInsertResult  :: [SQL]
  } deriving (Eq, Typeable)

data SqlDelete = SqlDelete
  { sqlDeleteFrom    :: String
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

emitClause :: (IsSQL sql) => String -> sql -> SQL
emitClause name sql =
  case toSQLCommand sql of
    SQL "" [] -> SQL "" []
    x -> SQL (" " ++ name ++ " ") [] `mappend` x

emitClausesSep :: String -> String -> [SQL] -> SQL
emitClausesSep _name _sep [] = SQL "" []
emitClausesSep name sep sqls =
  SQL (" " ++ name ++ " ") [] `mappend` mconcat (intersperse (SQL sep []) (filter outnull sqls))
  where
    outnull (SQL "" []) = False
    outnull _ = True

instance IsSQL SqlSelect where
  toSQLCommand cmd =
    SQL "SELECT " [] `mappend`
        emitClausesSep "" ", " (sqlSelectResult cmd) `mappend`
        emitClause "FROM" (sqlSelectFrom cmd) `mappend`
        emitClausesSep "WHERE" " AND " (sqlSelectWhere cmd) `mappend`
        emitClausesSep "GROUP BY" ", " (sqlSelectGroupBy cmd) `mappend`
        emitClausesSep "HAVING" " AND " (sqlSelectHaving cmd) `mappend`
        emitClausesSep "ORDER BY" ", " (sqlSelectOrderBy cmd) `mappend`
        (if sqlSelectOffset cmd > 0
           then SQL (" OFFSET " ++ show (sqlSelectOffset cmd)) []
           else SQL "" []) `mappend`
        (if sqlSelectLimit cmd >= 0
           then SQL (" LIMIT " ++ show (sqlSelectLimit cmd)) []
           else SQL "" [])

instance IsSQL SqlInsert where
  toSQLCommand cmd =
    SQL ("INSERT INTO " ++ sqlInsertWhat cmd) [] `mappend`
    SQL (" (" ++ (intercalate ", " (map fst (sqlInsertSet cmd)) ++ ")")) [] `mappend`
    SQL (" VALUES (") [] `mappend`
    mconcat (intersperse (SQL "," []) (map snd (sqlInsertSet cmd))) `mappend`
    SQL (")") [] `mappend`
    emitClausesSep "RETURNING" ", " (sqlInsertResult cmd)

instance IsSQL SqlUpdate where
  toSQLCommand cmd =
    SQL ("UPDATE " ++ sqlUpdateWhat cmd) [] `mappend`
    SQL " SET " [] `mappend`
    mconcat (intersperse (SQL ", " []) (map (\(name,value) -> (SQL (name ++ " = ") [] `mappend` value)) (sqlUpdateSet cmd))) `mappend`
    emitClause "FROM" (sqlUpdateFrom cmd) `mappend`
    emitClausesSep "WHERE" " AND " (sqlUpdateWhere cmd) `mappend`
    emitClausesSep "RETURNING" ", " (sqlUpdateResult cmd)

instance IsSQL SqlDelete where
  toSQLCommand cmd =
    SQL ("DELETE FROM " ++ (sqlDeleteFrom cmd)) [] `mappend`
        emitClausesSep "WHERE" " AND " (sqlDeleteWhere cmd)


sqlSelect :: String -> State SqlSelect () -> SqlSelect
sqlSelect table refine =
  execState refine (SqlSelect (SQL table []) [] [] [] [] [] 0 (-1))

sqlInsert :: String -> State SqlInsert () -> SqlInsert
sqlInsert table refine =
  execState refine (SqlInsert table mempty [])

sqlUpdate :: String -> State SqlUpdate () -> SqlUpdate
sqlUpdate table refine =
  execState refine (SqlUpdate table mempty [] [] [])

sqlDelete :: String -> State SqlDelete () -> SqlDelete
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

sqlWhere :: (MonadState v m, SqlWhere v, IsSQL sql) => sql -> m ()
sqlWhere sql = modify (\cmd -> sqlWhere1 cmd (toSQLCommand sql))

sqlWhereOr :: (MonadState v m, SqlWhere v, IsSQL sql) => [sql] -> m ()
sqlWhereOr [] = sqlWhere "FALSE"
sqlWhereOr [sql] = sqlWhere sql
sqlWhereOr sqls = sqlWhere $
                  SQL "(" [] `mappend`
                  mconcat (intersperse (SQL " OR " []) (map toSQLCommand sqls)) `mappend`
                  SQL ")" []

sqlWhereEq :: (MonadState v m, SqlWhere v, Convertible sql SqlValue) => String -> sql -> m ()
sqlWhereEq name value =
  sqlWhere (SQL (name ++ " = ?") [toSql value])

sqlWhereNotEq :: (MonadState v m, SqlWhere v, Convertible sql SqlValue) => String -> sql -> m ()
sqlWhereNotEq name value =
  sqlWhere (SQL (name ++ " <> ?") [toSql value])

sqlWhereLike :: (MonadState v m, SqlWhere v, Convertible sql SqlValue) => String -> sql -> m ()
sqlWhereLike name value =
  sqlWhere (SQL (name ++ " LIKE ?") [toSql value])

sqlWhereILike :: (MonadState v m, SqlWhere v, Convertible sql SqlValue) => String -> sql -> m ()
sqlWhereILike name value =
  sqlWhere (SQL (name ++ " ILIKE ?") [toSql value])

sqlWhereIn :: (MonadState v m, SqlWhere v, Convertible sql SqlValue) => String -> [sql] -> m ()
sqlWhereIn _name [] = sqlWhere (SQL "FALSE" [])
sqlWhereIn name [value] = sqlWhereEq name value
sqlWhereIn name values =
  sqlWhere (SQL (name ++ " IN (" ++ concat (intersperse "," (map (const "?") values)) ++ ")") (map toSql values))

sqlWhereNotIn :: (MonadState v m, SqlWhere v, Convertible sql SqlValue) => String -> [sql] -> m ()
sqlWhereNotIn _name [] = sqlWhere (SQL "TRUE" [])
sqlWhereNotIn name [value] = sqlWhereNotEq name value
sqlWhereNotIn name values =
  sqlWhere (SQL (name ++ " NOT IN (" ++ concat (intersperse "," (map (const "?") values)) ++ ")") (map toSql values))

sqlWhereExists :: (MonadState v m, SqlWhere v) => SqlSelect -> m ()
sqlWhereExists sqlSelectD = do
  sqlWhere (SQL "EXISTS (" [] `mappend` toSQLCommand (sqlSelectD { sqlSelectResult = [SQL "TRUE" []] }) `mappend` SQL ")" [])
  
sqlWhereIsNULL :: (MonadState v m, SqlWhere v) => String -> m ()
sqlWhereIsNULL col = sqlWhere (SQL (col ++ " IS NULL") [])

class SqlFrom a where
  sqlFrom1 :: a -> SQL -> a

instance SqlFrom SqlSelect where
  sqlFrom1 cmd sql = cmd { sqlSelectFrom = sqlSelectFrom cmd `mappend` sql }

instance SqlFrom SqlUpdate where
  sqlFrom1 cmd sql = cmd { sqlUpdateFrom = sqlUpdateFrom cmd `mappend` sql }

sqlFrom :: (MonadState v m, SqlFrom v, IsSQL sql) => sql -> m ()
sqlFrom sql = modify (\cmd -> sqlFrom1 cmd (toSQLCommand sql))

sqlJoin :: (MonadState v m, SqlFrom v, IsSQL sql) => sql -> m ()
sqlJoin table = sqlFrom (SQL ", " [] `mappend` toSQLCommand table)

sqlJoinOn :: (MonadState v m, SqlFrom v, IsSQL sql) => sql -> sql -> m ()
sqlJoinOn table condition = sqlFrom (SQL " JOIN " [] `mappend`
                                     toSQLCommand table `mappend`
                                     SQL " ON " [] `mappend`
                                     toSQLCommand condition)

sqlLeftJoinOn :: (MonadState v m, SqlFrom v, IsSQL sql) => sql -> sql -> m ()
sqlLeftJoinOn table condition = sqlFrom (SQL " LEFT JOIN " [] `mappend`
                                         toSQLCommand table `mappend`
                                         SQL " ON " [] `mappend`
                                         toSQLCommand condition)

sqlRightJoinOn :: (MonadState v m, SqlFrom v, IsSQL sql) => sql -> sql -> m ()
sqlRightJoinOn table condition = sqlFrom (SQL " RIGHT JOIN " [] `mappend`
                                          toSQLCommand table `mappend`
                                          SQL " ON " [] `mappend`
                                          toSQLCommand condition)

sqlFullJoinOn :: (MonadState v m, SqlFrom v, IsSQL sql) => sql -> sql -> m ()
sqlFullJoinOn table condition = sqlFrom (SQL " FULL JOIN " [] `mappend`
                                         toSQLCommand table `mappend`
                                         SQL " ON " [] `mappend`
                                         toSQLCommand condition)

class SqlSet a where
  sqlSet1 :: a -> String -> SQL -> a

instance SqlSet SqlUpdate where
  sqlSet1 cmd name sql = cmd { sqlUpdateSet = sqlUpdateSet cmd ++ [(name, sql)] }

instance SqlSet SqlInsert where
  sqlSet1 cmd name sql = cmd { sqlInsertSet = sqlInsertSet cmd ++ [(name, sql)] }


sqlSetCmd :: (MonadState v m, SqlSet v, IsSQL sql) => String -> sql -> m ()
sqlSetCmd name sql = modify (\cmd -> sqlSet1 cmd name (toSQLCommand sql))

sqlSet :: (MonadState v m, SqlSet v, Convertible sql SqlValue) => String -> sql -> m ()
sqlSet name sql = modify (\cmd -> sqlSet1 cmd name (SQL "?" [convert sql]))

class SqlResult a where
  sqlResult1 :: a -> SQL -> a

instance SqlResult SqlSelect where
  sqlResult1 cmd sql = cmd { sqlSelectResult = sqlSelectResult cmd ++ [sql] }

instance SqlResult SqlInsert where
  sqlResult1 cmd sql = cmd { sqlInsertResult = sqlInsertResult cmd ++ [sql] }

instance SqlResult SqlUpdate where
  sqlResult1 cmd sql = cmd { sqlUpdateResult = sqlUpdateResult cmd ++ [sql] }



sqlResult :: (MonadState v m, SqlResult v, IsSQL sql) => sql -> m ()
sqlResult sql = modify (\cmd -> sqlResult1 cmd (toSQLCommand sql))

class SqlOrderBy a where
  sqlOrderBy1 :: a -> SQL -> a

instance SqlOrderBy SqlSelect where
  sqlOrderBy1 cmd sql = cmd { sqlSelectOrderBy = sqlSelectOrderBy cmd ++ [sql] }



sqlOrderBy :: (MonadState v m, SqlOrderBy v, IsSQL sql) => sql -> m ()
sqlOrderBy sql = modify (\cmd -> sqlOrderBy1 cmd (toSQLCommand sql))

class SqlGroupByHaving a where
  sqlGroupBy1 :: a -> SQL -> a
  sqlHaving1 :: a -> SQL -> a

instance SqlGroupByHaving SqlSelect where
  sqlGroupBy1 cmd sql = cmd { sqlSelectGroupBy = sqlSelectGroupBy cmd ++ [sql] }
  sqlHaving1 cmd sql = cmd { sqlSelectHaving = sqlSelectHaving cmd ++ [sql] }


sqlGroupBy :: (MonadState v m, SqlGroupByHaving v, IsSQL sql) => sql -> m ()
sqlGroupBy sql = modify (\cmd -> sqlGroupBy1 cmd (toSQLCommand sql))

sqlHaving :: (MonadState v m, SqlGroupByHaving v, IsSQL sql) => sql -> m ()
sqlHaving sql = modify (\cmd -> sqlHaving1 cmd (toSQLCommand sql))


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
