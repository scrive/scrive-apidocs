module Chargeable.Migrations
    ( createIndexesForChargeableItems
    , createJointTypeCompanyIDTimeIndexForChargeableItems
    ) where

import Chargeable.Tables
import DB

createIndexesForChargeableItems :: MonadDB m => Migration m
createIndexesForChargeableItems = Migration {
    mgrTableName = tblName tableChargeableItems
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      let tname = tblName tableChargeableItems
      runQuery_ . sqlCreateIndex tname $ (indexOnColumn "\"time\"")
      runQuery_ . sqlCreateIndex tname $ (indexOnColumn "type")
  }


createJointTypeCompanyIDTimeIndexForChargeableItems :: MonadDB m => Migration m
createJointTypeCompanyIDTimeIndexForChargeableItems = Migration {
    mgrTableName = tblName tableChargeableItems
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      let tname = tblName tableChargeableItems
      runQuery_ . sqlCreateIndex tname $
                  (indexOnColumns  ["type", "company_id", "\"time\""])
  }
