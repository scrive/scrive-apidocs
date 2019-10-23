module ThirdPartyStats.Migrations
    (
      voidTableAsyncEventQueue
    , addPKToAsyncEventQueue
    ) where

import DB
import ThirdPartyStats.Tables

addPKToAsyncEventQueue :: MonadDB m => Migration m
addPKToAsyncEventQueue = Migration
  { mgrTableName = tblName tableAsyncEventQueue
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableAsyncEventQueue)
                       [ sqlAddPK (tblName tableAsyncEventQueue)
                                  (fromJust . pkOnColumn $ "sequence_number")
                       ]
  }

voidTableAsyncEventQueue :: MonadDB m => Migration m
voidTableAsyncEventQueue = Migration
  { mgrTableName = tblName tableAsyncEventQueue
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlDelete "async_event_queue" $ do
                       return ()
  }
