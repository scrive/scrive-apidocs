{-# LANGUAGE StrictData #-}
module Flow.Migrations where

import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Model

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

createTableFlowInstancesKVStore :: MonadDB m => Migration m
createTableFlowInstancesKVStore = Migration
  { mgrTableName = tblName tableFlowInstancesKVStore
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstancesKVStore
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
