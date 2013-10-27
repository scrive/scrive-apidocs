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
  , tblPrimaryKey = pkOnColumns ["email", "company_id"]
  , tblForeignKeys = [fkOnColumn "company_id" "companies" "id"]
  , tblIndexes = [
      indexOnColumn "company_id"
    , indexOnColumn "email"
    ]
  }
