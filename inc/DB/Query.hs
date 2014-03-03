module DB.Query (
    DBQuery(..)
  , DBUpdate(..)
  , dbQuery
  , dbUpdate
  ) where

import Database.PostgreSQL.PQTypes

-- query typeclasses
class MonadDB m => DBQuery m q r | q -> r where
  query :: q -> m r

class MonadDB m => DBUpdate m q r | q -> r where
  update :: q -> m r

dbQuery :: DBQuery m q r => q -> m r
dbQuery = query

dbUpdate :: DBUpdate m q r => q -> m r
dbUpdate = update
