module Payments.Migrations where

import DB
import Payments.Tables

addBillingEndDateCache :: MonadDB m => Migration m
addBillingEndDateCache =
  Migration {
    mgrTable = tablePaymentPlans
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE payment_plans ADD COLUMN billing_ends TIMESTAMPTZ NULL"
      kRunRaw "UPDATE payment_plans SET billing_ends = now() + interval '30 days'"
      kRunRaw "ALTER TABLE payment_plans ALTER COLUMN billing_ends SET NOT NULL"
  }

attachUniqueContraintsToPaymentPlansColumns :: MonadDB m => Migration m
attachUniqueContraintsToPaymentPlansColumns =
  Migration {
    mgrTable = tablePaymentPlans
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "ALTER TABLE payment_plans ADD UNIQUE (user_id), ADD UNIQUE (company_id);"
      kRunRaw "DROP INDEX idx_payment_plans_user_id;"
      kRunRaw "DROP INDEX idx_payment_plans_company_id;"
  }

paymentsPlansOnlyForCompanies :: MonadDB m => Migration m
paymentsPlansOnlyForCompanies =
  Migration {
    mgrTable = tablePaymentPlans
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw "ALTER TABLE payment_plans DROP CONSTRAINT IF EXISTS check_payment_plans_type_id"
      kRunRaw "ALTER TABLE payment_plans DROP CONSTRAINT IF EXISTS ch_payment_plans_type_id" -- old name
      kRunRaw "UPDATE payment_plans SET company_id = (SELECT company_id FROM users WHERE user_id = users.id) WHERE company_id IS NULL;"
      kRunRaw "ALTER TABLE payment_plans DROP COLUMN account_type"
      kRunRaw "ALTER TABLE payment_plans DROP COLUMN user_id"
  }

removeDuplicateIndexFromPaymentPlans :: MonadDB m => Migration m
removeDuplicateIndexFromPaymentPlans = Migration {
  mgrTable = tablePaymentPlans
, mgrFrom = 4
, mgrDo = do
  let Table{..} = tablePaymentPlans
  kRun_ . sqlDropIndex tblName $ indexOnColumn "company_id"
  -- additionally drop unique constraint and create unique index on company_id
  kRun_ $ sqlAlterTable tblName ["DROP CONSTRAINT IF EXISTS unique_payment_plans_company_id"]
  kRun_ . sqlCreateIndex tblName $ uniqueIndexOnColumn "company_id"
}

paymentsStatsOnlyForCompanies :: MonadDB m => Migration m
paymentsStatsOnlyForCompanies =
  Migration {
    mgrTable = tablePaymentStats
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE payment_stats DROP CONSTRAINT IF EXISTS check_payment_stats_type_id"
      kRunRaw "ALTER TABLE payment_stats DROP CONSTRAINT IF EXISTS ch_payment_stats_type_id"   -- old name
      kRunRaw "ALTER TABLE payment_stats DROP CONSTRAINT IF EXISTS ch_payment_statss_type_id"  -- old name, with spelling error
      kRunRaw "UPDATE payment_stats SET company_id = (SELECT company_id FROM users WHERE user_id = users.id) WHERE company_id IS NULL;"
      kRunRaw "ALTER TABLE payment_stats DROP COLUMN account_type"
      kRunRaw "ALTER TABLE payment_stats DROP COLUMN user_id"
  }
