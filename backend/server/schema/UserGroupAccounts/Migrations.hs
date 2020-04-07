module UserGroupAccounts.Migrations where

import DB
import UserGroupAccounts.Tables

changeCompanyToUserGroupInCompanyInvites :: MonadDB m => Migration m
changeCompanyToUserGroupInCompanyInvites = Migration
  { mgrTableName = tblName tableCompanyInvites
  , mgrFrom      = 2
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableCompanyInvites

      -- Set up everything related to new user_group_id
      runQuery_ $ sqlAlterTable
        tname
        [ sqlAddColumn $ tblColumn { colName     = "user_group_id"
                                   , colType     = BigIntT
                                   , colNullable = True
                                   }
        ]
      runQuery_ . sqlUpdate "companyinvites" $ do
        sqlSetCmd "user_group_id" "company_id"
      runQuery_ $ sqlAlterTable
        tname
        [ sqlAlterColumn "user_group_id" "SET NOT NULL"
        , sqlAddValidFK tname (fkOnColumn "user_group_id" "user_groups" "id")
        ]
      runQuery_ $ sqlCreateIndexSequentially tname (indexOnColumn "user_group_id")
      runQuery_ $ sqlAlterTable
        tname
        [ sqlDropPK tname
        , sqlAddPK tname (fromJust . pkOnColumns $ ["user_group_id", "user_id"])
        ]

      -- Drop everything related to company_id column
      runQuery_ $ sqlDropIndex tname (indexOnColumn "company_id")
      runQuery_ $ sqlAlterTable
        tname
        [ sqlDropFK tname (fkOnColumn "company_id" "companies" "id")
        , sqlDropColumn "company_id"
        ]
  }
