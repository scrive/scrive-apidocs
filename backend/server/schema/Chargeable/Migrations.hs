module Chargeable.Migrations
    ( createIndexesForChargeableItems
    , createJointTypeCompanyIDTimeIndexForChargeableItems
    , chargeableItemsAddUserGroupID
    , createJointTypeUserGroupIDTimeIndexForChargeableItems
    , dropFKCascadeForUserGroupID
    , dropCompanyIDForChargeableItems
    ) where

import Control.Monad.Catch

import Chargeable.Tables
import DB

createIndexesForChargeableItems :: MonadDB m => Migration m
createIndexesForChargeableItems = Migration
  { mgrTableName = tblName tableChargeableItems
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableChargeableItems
                     runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "\"time\""
                     runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "type"
  }


createJointTypeCompanyIDTimeIndexForChargeableItems :: MonadDB m => Migration m
createJointTypeCompanyIDTimeIndexForChargeableItems = Migration
  { mgrTableName = tblName tableChargeableItems
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableChargeableItems
                     runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumns
                       ["type", "company_id", "\"time\""]
  }

chargeableItemsAddUserGroupID :: (MonadThrow m, MonadDB m) => Migration m
chargeableItemsAddUserGroupID = Migration
  { mgrTableName = tblName tableChargeableItems
  , mgrFrom      = 3
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableChargeableItems
      runQuery_ $ sqlAlterTable
        tname
        [ sqlAddColumn
          $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
        , sqlAddValidFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
          { fkOnDelete = ForeignKeySetNull
          }
        ]
      runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "user_group_id"
  }

createJointTypeUserGroupIDTimeIndexForChargeableItems :: MonadDB m => Migration m
createJointTypeUserGroupIDTimeIndexForChargeableItems = Migration
  { mgrTableName = tblName tableChargeableItems
  , mgrFrom      = 4
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableChargeableItems
                     runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumns
                       ["type", "user_group_id", "\"time\""]
  }

dropFKCascadeForUserGroupID :: MonadDB m => Migration m
dropFKCascadeForUserGroupID = Migration
  { mgrTableName = tblName tableChargeableItems
  , mgrFrom      = 5
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableChargeableItems
                     runQuery_ $ sqlAlterTable
                       tname
                       [ sqlDropFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
                         { fkOnDelete = ForeignKeySetNull
                         }
                       , sqlAddValidFK tname $ fkOnColumn "user_group_id" "user_groups" "id"
                       ]
  }


dropCompanyIDForChargeableItems :: MonadDB m => Migration m
dropCompanyIDForChargeableItems = Migration
  { mgrTableName = tblName tableChargeableItems
  , mgrFrom      = 6
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableChargeableItems
                     runQuery_ $ sqlDropIndex
                       tname
                       (indexOnColumns ["type", "company_id", "\"time\""])
                     runQuery_ $ sqlDropIndex tname (indexOnColumn "company_id")
                     runQuery_ $ sqlAlterTable
                       tname
                       [ sqlDropFK tname (fkOnColumn "company_id" "companies" "id")
                       , sqlDropColumn "company_id"
                       ]
  }
