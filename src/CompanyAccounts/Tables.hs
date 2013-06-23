module CompanyAccounts.Tables (
    tableCompanyInvites
  ) where

import DB

tableCompanyInvites :: Table
tableCompanyInvites = tblTable {
    tblName = "companyinvites"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "email", colType = TextT, colNullable = False }
    , tblColumn { colName = "first_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "last_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "company_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["email", "company_id"]
  , tblForeignKeys = [tblForeignKeyColumn "company_id" "companies" "id"]
  , tblIndexes = [
      tblIndexOnColumn "company_id"
    , tblIndexOnColumn "email"
    ]
  }
