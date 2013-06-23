module ScriveByMail.Tables (
    tableUserMailAPIs
  , tableCompanyMailAPIs
  ) where

import DB

tableUserMailAPIs :: Table
tableUserMailAPIs = tblTable {
    tblName = "user_mail_apis"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "key", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "daily_limit", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "sent_today", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "last_sent_date", colType = DateT, colNullable = False }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tableCompanyMailAPIs :: Table
tableCompanyMailAPIs = tblTable {
    tblName = "company_mail_apis"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "company_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "key", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "daily_limit", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "sent_today", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "last_sent_date", colType = DateT, colNullable = False }
    ]
  , tblPrimaryKey = ["company_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
