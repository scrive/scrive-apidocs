module User.CallbackScheme.Tables where

import DB

tableUsersCallbackScheme :: Table
tableUsersCallbackScheme = tblTable
  { tblName        = "user_callback_scheme"
  , tblVersion     = 3
  , tblColumns     =
    [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "callback_scheme", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "user_id"
  , tblForeignKeys =
    [(fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }]
  }
