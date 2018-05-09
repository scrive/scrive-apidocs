module User.Tables where

import DB

tableUsers :: Table
tableUsers = tblTable {
    tblName = "users"
  , tblVersion = 26
  , tblVersion = 24
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "password", colType = BinaryT, colNullable = False }
      , tblColumn { colName = "salt", colType = BinaryT, colNullable = False }
      , tblColumn { colName = "is_company_admin", colType = BoolT, colNullable = False }
      , tblColumn { colName = "account_suspended", colType = BoolT, colNullable = False }
      , tblColumn { colName = "has_accepted_terms_of_service", colType = TimestampWithZoneT }
      , tblColumn { colName = "signup_method", colType = SmallIntT, colNullable = False }
      , tblColumn { colName = "first_name", colType = TextT, colNullable = False }
      , tblColumn { colName = "last_name", colType = TextT, colNullable = False }
      , tblColumn { colName = "personal_number", colType = TextT, colNullable = False }
      , tblColumn { colName = "company_position", colType = TextT, colNullable = False }
      , tblColumn { colName = "phone", colType = TextT, colNullable = False }
      , tblColumn { colName = "email", colType = TextT, colNullable = False }
      , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False  }
      , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
      , tblColumn { colName = "associated_domain_id", colType = BigIntT , colNullable = False }
      , tblColumn { colName = "password_algorithm", colType = SmallIntT, colNullable = True }
      , tblColumn { colName = "totp_key", colType = BinaryT, colNullable = True }
      , tblColumn { colName = "totp_active", colType = BoolT, colNullable = False, colDefault = Just "false" }
      , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
      ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblChecks =
      [ Check "check_users_lowercase_email" "email = lower(email)"
      ]
  , tblForeignKeys = [
      fkOnColumn "user_group_id" "user_groups" "id"
    , fkOnColumn "associated_domain_id" "branded_domains" "id"
    ]
  , tblIndexes = [
      indexOnColumn "user_group_id"
    , (indexOnColumn "email") { idxUnique = True, idxWhere = Just ("deleted IS NULL") }
    ]
  }
