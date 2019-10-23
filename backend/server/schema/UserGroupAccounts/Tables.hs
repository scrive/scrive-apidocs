module UserGroupAccounts.Tables (
    tableCompanyInvites
  ) where

import DB

tableCompanyInvites :: Table
tableCompanyInvites = tblTable
  { tblName        = "companyinvites"
  , tblVersion     = 3
  , tblColumns     =
    [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumns ["user_group_id", "user_id"]
  , tblForeignKeys = [ fkOnColumn "user_id"       "users"       "id"
                     , fkOnColumn "user_group_id" "user_groups" "id"
                     ]
  , tblIndexes     = [indexOnColumn "user_id", indexOnColumn "user_group_id"]
  }
