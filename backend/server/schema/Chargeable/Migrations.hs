module Chargeable.Migrations (createIndexesForChargeableItems) where

import Chargeable.Tables
import DB
import KontraPrelude

createIndexesForChargeableItems :: MonadDB m => Migration m
createIndexesForChargeableItems = Migration {
    mgrTableName = tblName tableChargeableItems
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      let tname = tblName tableChargeableItems
      runQuery_ . sqlCreateIndex tname $ (indexOnColumn "\"time\"")
      runQuery_ . sqlCreateIndex tname $ (indexOnColumn "type")
  }
