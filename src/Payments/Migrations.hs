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
