module Payments.Tables where

import Data.Monoid

import DB

{-
  A payment plan relates a User or Company to a Recurly Account_Code
  and remembers the plan they are using.
-}
tablePaymentPlans :: Table
tablePaymentPlans = tblTable {
    tblName = "payment_plans"
  , tblVersion = 3
  , tblColumns = [
      tblColumn { colName = "account_code", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "account_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT }
    , tblColumn { colName = "company_id", colType = BigIntT }
    , tblColumn { colName = "plan", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "plan_pending", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "status", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "status_pending", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "quantity", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "quantity_pending", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "sync_date", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "dunning_step", colType = SmallIntT }
    , tblColumn { colName = "dunning_date", colType = TimestampWithZoneT }
    , tblColumn { colName = "billing_ends", colType = TimestampWithZoneT, colNullable = False }
    ]
  , tblPrimaryKey = ["account_code"]
  , tblChecks = [TableCheck "type_id" $ mconcat [
        "account_type = 1 AND user_id IS NOT NULL AND company_id IS NULL" -- user acc
      , " OR "
      , "account_type = 2 AND company_id IS NOT NULL AND user_id IS NULL" -- company acc
      ]
    ]
  , tblUniques = [["user_id"], ["company_id"]]
  , tblForeignKeys = [
      tblForeignKeyColumn "user_id" "users" "id"
    , tblForeignKeyColumn "company_id" "companies" "id"
    ]
  }

tablePaymentStats :: Table
tablePaymentStats = tblTable {
    tblName = "payment_stats"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "action", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "quantity", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "plan", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "account_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT }
    , tblColumn { colName = "company_id", colType = BigIntT }
    , tblColumn { colName = "account_code", colType = BigIntT, colNullable = False }
    ]
  , tblChecks = [TableCheck "type_id" $ mconcat [
        "account_type = 1 AND user_id IS NOT NULL AND company_id IS NULL" -- user acc
      , " OR "
      , "account_type = 2 AND company_id IS NOT NULL AND user_id IS NULL" -- company acc
      ]
    ]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
