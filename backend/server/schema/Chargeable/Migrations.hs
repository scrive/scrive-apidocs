module Chargeable.Migrations
    ( createIndexesForChargeableItems
    , createJointTypeCompanyIDTimeIndexForChargeableItems
    , chargeableItemsAddUserGroupID
    ) where

import Control.Monad.Catch

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

chargeableItemsAddUserGroupID :: (MonadThrow m, MonadDB m) => Migration m
chargeableItemsAddUserGroupID = Migration {
  mgrTableName = tblName tableChargeableItems
, mgrFrom = 3
, mgrAction = StandardMigration $ do
    let tname = tblName tableChargeableItems
    runQuery_ $ sqlAlterTable tname
      [
        sqlAddColumn $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
      , sqlAddFK tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
      ]
    runQuery_ . sqlCreateIndex tname $ indexOnColumn "user_group_id"
}
