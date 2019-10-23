module Theme.Migrations where

import Control.Monad.Catch

import DB
import Theme.Tables

themeOwnershipAddUserGroupID :: (MonadThrow m, MonadDB m) => Migration m
themeOwnershipAddUserGroupID = Migration
  { mgrTableName = tblName tableThemeOwnership
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ runQuery_ $ do
      sqlAlterTable
        (tblName tableThemeOwnership)
        [ sqlAddColumn
          $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
        , sqlAddValidFK (tblName tableThemeOwnership)
          $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull
                                                            }
        ]
  }

themeOwnershipMakeUserGroupIDMandatory :: (MonadThrow m, MonadDB m) => Migration m
themeOwnershipMakeUserGroupIDMandatory = Migration
  { mgrTableName = tblName tableThemeOwnership
  , mgrFrom      = 2
  , mgrAction    =
    StandardMigration $ runQuery_ $ do
      let tname = tblName tableThemeOwnership
      sqlAlterTable
        tname
        [ sqlDropFK tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull
                                                                            }
        , sqlAddValidFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
          { fkOnDelete = ForeignKeyCascade
          }
        , sqlDropFK tname
          $ (fkOnColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade }
        , sqlAddValidFK tname
          $ (fkOnColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyNoAction }
        , sqlDropCheck "check_theme_is_owned_by_company_or_domain"
        , sqlAddValidCheck $ tblCheck
          { chkName      = "check_theme_is_owned_by_user_group_or_domain"
          , chkCondition =
            "(company_id IS \  \NULL AND user_group_id IS \  \NULL OR domain_id IS \  \NULL) \
            \AND (company_id IS NOT NULL AND user_group_id IS NOT NULL OR domain_id IS NOT NULL)" -- (usergroup && company) XOR domain
          }
        ]
  }


themeOwnershipDropCompanyID :: (MonadThrow m, MonadDB m) => Migration m
themeOwnershipDropCompanyID = Migration
  { mgrTableName = tblName tableThemeOwnership
  , mgrFrom      = 3
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableThemeOwnership
      runQuery_ $ sqlAlterTable
        tname
        [ sqlDropFK tname $ (fkOnColumn "company_id" "companies" "id")
        , sqlDropCheck "check_theme_is_owned_by_user_group_or_domain"
        ]
      runQuery_ $ sqlAlterTable
        tname
        [ sqlAddValidCheck $ tblCheck
            { chkName      = "check_theme_is_owned_by_user_group_or_domain"
            , chkCondition =
              "(user_group_id IS \  \NULL OR domain_id IS \  \NULL) \
            \AND (user_group_id IS NOT NULL OR domain_id IS NOT NULL)" -- (usergroup && company) XOR domain
            }
        ]
      runQuery_ $ sqlAlterTable tname [sqlDropColumn "company_id"]
  }
