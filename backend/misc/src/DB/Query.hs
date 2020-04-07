module DB.Query
  ( DBQuery(..)
  , DBUpdate(..)
  ) where

import Database.PostgreSQL.PQTypes

-- query typeclasses
class MonadDB m => DBQuery m q r | q -> r where
  dbQuery :: q -> m r

class MonadDB m => DBUpdate m q r | q -> r where
  dbUpdate :: q -> m r
