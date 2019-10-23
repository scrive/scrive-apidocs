module User.PasswordReminder.Tables where

import DB

tablePasswordReminders :: Table
tablePasswordReminders = tblTable
  { tblName        = "password_reminders"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "remained_emails", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "user_id"
  , tblForeignKeys =
    [(fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }]
  }
