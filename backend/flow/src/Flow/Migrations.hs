{-# LANGUAGE StrictData #-}
module Flow.Migrations where

import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Model
import Database.PostgreSQL.PQTypes.Utils

import Flow.Tables

createTableFlowTemplates :: MonadDB m => Migration m
createTableFlowTemplates = Migration
  { mgrTableName = tblName tableFlowTemplates
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowTemplates
  }

createTableFlowInstances :: MonadDB m => Migration m
createTableFlowInstances = Migration
  { mgrTableName = tblName tableFlowInstances
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstances
  }

createTableFlowInstanceKeyValueStore :: MonadDB m => Migration m
createTableFlowInstanceKeyValueStore = Migration
  { mgrTableName = tblName tableFlowInstanceKeyValueStore
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstanceKeyValueStore
  }

createTableFlowInstanceSignatories :: MonadDB m => Migration m
createTableFlowInstanceSignatories = Migration
  { mgrTableName = tblName tableFlowInstanceSignatories
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstanceSignatories
  }

createTableFlowEvents :: MonadDB m => Migration m
createTableFlowEvents = Migration
  { mgrTableName = tblName tableFlowEvents
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowEvents
  }

createTableFlowAggregatorEvents :: MonadDB m => Migration m
createTableFlowAggregatorEvents = Migration
  { mgrTableName = tblName tableFlowAggregatorEvents
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowAggregatorEvents
  }

createTableFlowInstanceAccessTokens :: MonadDB m => Migration m
createTableFlowInstanceAccessTokens = Migration
  { mgrTableName = tblName tableFlowInstanceAccessTokens
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstanceAccessTokens
  }

createTableFlowInstanceSessions :: MonadDB m => Migration m
createTableFlowInstanceSessions = Migration
  { mgrTableName = tblName tableFlowInstanceSessions
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstanceSessions
  }

addIndicesToFlowInstanceKeyValueStore :: MonadDB m => Migration m
addIndicesToFlowInstanceKeyValueStore = Migration
  { mgrTableName = tableName
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlCreateIndexSequentially tableName $ indexOnColumn
                       "string"
                     runQuery_ . sqlCreateIndexSequentially tableName $ indexOnColumn
                       "user_id"
  }
  where tableName = tblName tableFlowInstanceKeyValueStore
