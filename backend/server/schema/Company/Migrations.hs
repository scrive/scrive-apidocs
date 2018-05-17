module Company.Migrations (
    companiesAddPartnerID
  , companiesAddPadAppModeAndEArchiveEnabled
  , companiesAddPaymentPlan
  , companiesDropAllowSaveSafetyCopy
) where

import Control.Monad.Catch

import Company.Tables
import DB

companiesAddPartnerID :: (MonadThrow m, MonadDB m) => Migration m
companiesAddPartnerID = Migration {
  mgrTableName = tblName tableCompanies
, mgrFrom = 20
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableCompanies)
                              [ sqlAddColumn $ tblColumn
                                                 { colName = "partner_id"
                                                 , colType = BigIntT
                                                 , colNullable = True
                                                 }
                              , sqlAddFK (tblName tableCompanies) $
                                    (fkOnColumn "partner_id" "partners" "id" )
                                      { fkOnDelete = ForeignKeySetNull } ]
    runSQL_ "UPDATE companies SET partner_id = (SELECT partners.id FROM partners WHERE partners.default_partner) WHERE partner_id IS NULL"
    runSQL_ "ALTER TABLE companies ALTER partner_id SET NOT NULL"
}

companiesAddPaymentPlan :: (MonadThrow m, MonadDB m) => Migration m
companiesAddPaymentPlan = Migration {
  mgrTableName = tblName tableCompanies
, mgrFrom = 22
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableCompanies)  [ sqlAddColumn $
        tblColumn { colName = "payment_plan", colType = SmallIntT, colNullable = True, colDefault = Just "0"}
      ]
    -- Migrate One plan to One plan
    runSQL_ "UPDATE companies SET payment_plan = 1 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 6)"
    -- Migrate Team plan to Team plan
    runSQL_ "UPDATE companies SET payment_plan = 2 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 1)"
    -- Migrate Form, Company and Enterprise plans to Enterprise plan
    runSQL_ "UPDATE companies SET payment_plan = 3 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 2 OR plan = 3 OR plan = 5)"
    -- Migrate Trial plan to Trial plan
    runSQL_ "UPDATE companies SET payment_plan = 4 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 4)"
    runSQL_ "ALTER TABLE companies ALTER COLUMN payment_plan SET NOT NULL"
    runSQL_ "DROP TABLE payment_plans"
    runSQL_ "DELETE FROM table_versions WHERE name='payment_plans'"
}

companiesAddPadAppModeAndEArchiveEnabled :: MonadDB m => Migration m
companiesAddPadAppModeAndEArchiveEnabled = Migration {
  mgrTableName = tblName tableCompanies
, mgrFrom = 21
, mgrAction = StandardMigration $ runQuery_ $ sqlAlterTable (tblName tableCompanies) [
    sqlAddColumn $ tblColumn { colName = "pad_app_mode", colType = SmallIntT, colNullable = False, colDefault = Just "1" }
  , sqlAddColumn $ tblColumn { colName = "pad_earchive_enabled", colType = BoolT, colNullable = False, colDefault = Just "true" }
  ]
}

companiesDropAllowSaveSafetyCopy :: (MonadThrow m, MonadDB m) => Migration m
companiesDropAllowSaveSafetyCopy = Migration {
  mgrTableName = tblName tableCompanies
, mgrFrom = 23
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableCompanies) [ sqlDropColumn "allow_save_safety_copy" ]
}
