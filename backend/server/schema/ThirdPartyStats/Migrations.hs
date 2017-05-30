module ThirdPartyStats.Migrations
    (
     voidTableAsyncEventQueue
    ) where

import DB
import KontraPrelude
import ThirdPartyStats.Tables

voidTableAsyncEventQueue :: MonadDB m => Migration m
voidTableAsyncEventQueue = Migration {
    mgrTableName = tblName tableAsyncEventQueue
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runQuery_ . sqlDelete "async_event_queue" $ do
        return ()
  }
