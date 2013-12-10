module Payments.Migrations where

import DB
import Payments.Tables

addBillingEndDateCache :: MonadDB m => Migration m
addBillingEndDateCache =
  Migration {
    mgrTable = tablePaymentPlans
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE payment_plans ADD COLUMN billing_ends TIMESTAMPTZ NULL"
      runSQL_ "UPDATE payment_plans SET billing_ends = now() + interval '30 days'"
      runSQL_ "ALTER TABLE payment_plans ALTER COLUMN billing_ends SET NOT NULL"
  }

attachUniqueContraintsToPaymentPlansColumns :: MonadDB m => Migration m
attachUniqueContraintsToPaymentPlansColumns =
  Migration {
    mgrTable = tablePaymentPlans
  , mgrFrom = 2
  , mgrDo = do
      runSQL_ "ALTER TABLE payment_plans ADD UNIQUE (user_id), ADD UNIQUE (company_id);"
      runSQL_ "DROP INDEX idx_payment_plans_user_id;"
      runSQL_ "DROP INDEX idx_payment_plans_company_id;"
  }

paymentsPlansOnlyForCompanies :: MonadDB m => Migration m
paymentsPlansOnlyForCompanies =
  Migration {
    mgrTable = tablePaymentPlans
  , mgrFrom = 3
  , mgrDo = do
      runSQL_ "ALTER TABLE payment_plans DROP CONSTRAINT IF EXISTS check_payment_plans_type_id"
      runSQL_ "ALTER TABLE payment_plans DROP CONSTRAINT IF EXISTS ch_payment_plans_type_id" -- old name
      runSQL_ "UPDATE payment_plans SET company_id = (SELECT company_id FROM users WHERE user_id = users.id) WHERE company_id IS NULL;"
      runSQL_ "ALTER TABLE payment_plans DROP COLUMN account_type"
      runSQL_ "ALTER TABLE payment_plans DROP COLUMN user_id"
  }

removeDuplicateIndexFromPaymentPlans :: MonadDB m => Migration m
removeDuplicateIndexFromPaymentPlans = Migration {
  mgrTable = tablePaymentPlans
, mgrFrom = 4
, mgrDo = do
  let Table{..} = tablePaymentPlans
  runQuery_ . sqlDropIndex tblName $ indexOnColumn "company_id"
  -- additionally drop unique constraint and create unique index on company_id
  runQuery_ $ sqlAlterTable tblName ["DROP CONSTRAINT IF EXISTS unique_payment_plans_company_id"]
  runQuery_ . sqlCreateIndex tblName $ uniqueIndexOnColumn "company_id"
}

paymentsStatsOnlyForCompanies :: MonadDB m => Migration m
paymentsStatsOnlyForCompanies =
  Migration {
    mgrTable = tablePaymentStats
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE payment_stats DROP CONSTRAINT IF EXISTS check_payment_stats_type_id"
      runSQL_ "ALTER TABLE payment_stats DROP CONSTRAINT IF EXISTS ch_payment_stats_type_id"   -- old name
      runSQL_ "ALTER TABLE payment_stats DROP CONSTRAINT IF EXISTS ch_payment_statss_type_id"  -- old name, with spelling error
      runSQL_ "UPDATE payment_stats SET company_id = (SELECT company_id FROM users WHERE user_id = users.id) WHERE company_id IS NULL;"
      runSQL_ "ALTER TABLE payment_stats DROP COLUMN account_type"
      runSQL_ "ALTER TABLE payment_stats DROP COLUMN user_id"
  }
