module CompanyAccounts.Tables (
    tableCompanyInvites
  ) where

import DB

tableCompanyInvites :: Table
tableCompanyInvites = tblTable {
    tblName = "companyinvites"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "company_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumns ["company_id","user_id"]
  , tblForeignKeys = [
      fkOnColumn "company_id" "companies" "id"
    , fkOnColumn "user_id" "users" "id"
    ]
  , tblIndexes = [
      indexOnColumn "company_id"
    , indexOnColumn "user_id"
    ]
  }
