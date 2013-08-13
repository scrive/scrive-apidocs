module Payments.Tables where

import DB

{-
  A payment plan relates a User or Company to a Recurly Account_Code
  and remembers the plan they are using.
-}
tablePaymentPlans :: Table
tablePaymentPlans = tblTable {
    tblName = "payment_plans"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "account_code", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "company_id", colType = BigIntT, colNullable = False  }
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
  , tblChecks = []
  , tblUniques = [["company_id"]]
  , tblForeignKeys = [
      tblForeignKeyColumn "company_id" "companies" "id"
    ]
  }

tablePaymentStats :: Table
tablePaymentStats = tblTable {
    tblName = "payment_stats"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "action", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "quantity", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "plan", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "company_id", colType = BigIntT , colNullable = False}
    , tblColumn { colName = "account_code", colType = BigIntT, colNullable = False }
    ]
  , tblChecks = []
  , tblForeignKeys = [
      (tblForeignKeyColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
