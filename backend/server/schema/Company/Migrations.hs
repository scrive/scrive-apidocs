module Company.Migrations (
    companiesAddPartnerID
  , companiesAddPadAppModeAndEArchiveEnabled
  , companiesAddPaymentPlan
  , companiesDropAllowSaveSafetyCopy
  , companiesAddUserGroupID
  , companiesMakeUserGroupIDNotNull
  , companiesDropTable
  , companyUIsDropTable
) where

import Control.Monad.Catch

import DB

tableCompaniesName :: RawSQL ()
tableCompaniesName = "companies"

tableCompanyUIsName :: RawSQL ()
tableCompanyUIsName = "company_uis"

companiesAddPartnerID :: (MonadThrow m, MonadDB m) => Migration m
companiesAddPartnerID = Migration
  { mgrTableName = tableCompaniesName
  , mgrFrom      = 20
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        tableCompaniesName
        [ sqlAddColumn
          $ tblColumn { colName = "partner_id", colType = BigIntT, colNullable = True }
        , sqlAddValidFK tableCompaniesName
          $ (fkOnColumn "partner_id" "partners" "id") { fkOnDelete = ForeignKeySetNull }
        ]
      runSQL_
        "UPDATE companies SET partner_id = (SELECT partners.id FROM partners WHERE partners.default_partner) WHERE partner_id IS NULL"
      runSQL_ "ALTER TABLE companies ALTER partner_id SET NOT NULL"
  }

companiesAddPadAppModeAndEArchiveEnabled :: MonadDB m => Migration m
companiesAddPadAppModeAndEArchiveEnabled = Migration
  { mgrTableName = tableCompaniesName
  , mgrFrom      = 21
  , mgrAction    = StandardMigration . runQuery_ $ sqlAlterTable
                     tableCompaniesName
                     [ sqlAddColumn $ tblColumn { colName     = "pad_app_mode"
                                                , colType     = SmallIntT
                                                , colNullable = False
                                                , colDefault  = Just "1"
                                                }
                     , sqlAddColumn $ tblColumn { colName     = "pad_earchive_enabled"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "true"
                                                }
                     ]
  }

companiesAddPaymentPlan :: (MonadThrow m, MonadDB m) => Migration m
companiesAddPaymentPlan = Migration
  { mgrTableName = tableCompaniesName
  , mgrFrom      = 22
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        tableCompaniesName
        [ sqlAddColumn $ tblColumn { colName     = "payment_plan"
                                   , colType     = SmallIntT
                                   , colNullable = True
                                   , colDefault  = Just "0"
                                   }
        ]
      -- Migrate One plan to One plan
      runSQL_
        "UPDATE companies SET payment_plan = 1 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 6)"
      -- Migrate Team plan to Team plan
      runSQL_
        "UPDATE companies SET payment_plan = 2 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 1)"
      -- Migrate Form, Company and Enterprise plans to Enterprise plan
      runSQL_
        "UPDATE companies SET payment_plan = 3 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 2 OR plan = 3 OR plan = 5)"
      -- Migrate Trial plan to Trial plan
      runSQL_
        "UPDATE companies SET payment_plan = 4 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 4)"
      runSQL_ "ALTER TABLE companies ALTER COLUMN payment_plan SET NOT NULL"
      runSQL_ "DROP TABLE payment_plans"
      runSQL_ "DELETE FROM table_versions WHERE name='payment_plans'"
  }

companiesDropAllowSaveSafetyCopy :: (MonadThrow m, MonadDB m) => Migration m
companiesDropAllowSaveSafetyCopy = Migration
  { mgrTableName = tableCompaniesName
  , mgrFrom      = 23
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable tableCompaniesName
                                               [sqlDropColumn "allow_save_safety_copy"]
  }

companiesAddUserGroupID :: MonadDB m => Migration m
companiesAddUserGroupID = Migration
  { mgrTableName = tableCompaniesName
  , mgrFrom      = 24
  , mgrAction    =
    StandardMigration $ do
      let tname = tableCompaniesName
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

companiesMakeUserGroupIDNotNull :: MonadDB m => Migration m
companiesMakeUserGroupIDNotNull = Migration
  { mgrTableName = tableCompaniesName
  , mgrFrom      = 25
  , mgrAction    =
    StandardMigration $ do
      let tname = tableCompaniesName
      runQuery_
        . sqlAlterTable tname
        $ [ sqlAlterColumn "user_group_id" "SET NOT NULL"
          , sqlDropFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
            { fkOnDelete = ForeignKeySetNull
            }
          , sqlAddValidFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
            { fkOnDelete = ForeignKeyCascade
            }
          ]
  }

companiesDropTable :: MonadDB m => Migration m
companiesDropTable = Migration { mgrTableName = tableCompaniesName
                               , mgrFrom      = 26
                               , mgrAction    = DropTableMigration DropTableRestrict
                               }

companyUIsDropTable :: MonadDB m => Migration m
companyUIsDropTable = Migration { mgrTableName = tableCompanyUIsName
                                , mgrFrom      = 4
                                , mgrAction    = DropTableMigration DropTableRestrict
                                }
