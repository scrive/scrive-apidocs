module User.Tables where

import DB

checkMandatoryFieldsUnlessDetected23 :: RawSQL ()
checkMandatoryFieldsUnlessDetected23 =
  "deleted IS NULL AND first_name IS NOT NULL \
  \AND last_name IS NOT NULL AND personal_number IS NOT NULL \
  \AND company_position IS NOT NULL AND phone IS NOT NULL \
  \AND email IS NOT NULL AND lang IS NOT NULL OR \
  \deleted IS NOT NULL AND first_name IS NULL \
  \AND last_name IS NULL AND personal_number IS NULL \
  \AND company_position IS NULL AND phone IS NULL AND email IS NULL \
  \AND lang IS NULL"

tableUsers :: Table
tableUsers = tblTable {
    tblName = "users"
  , tblVersion = 26
  , tblVersion = 24
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "password", colType = BinaryT }
      , tblColumn { colName = "salt", colType = BinaryT }
      , tblColumn { colName = "is_company_admin", colType = BoolT, colNullable = False }
      , tblColumn { colName = "account_suspended", colType = BoolT, colNullable = False }
      , tblColumn { colName = "has_accepted_terms_of_service", colType = TimestampWithZoneT }
      , tblColumn { colName = "signup_method", colType = SmallIntT, colNullable = False }
      , tblColumn { colName = "first_name", colType = TextT }
      , tblColumn { colName = "last_name", colType = TextT }
      , tblColumn { colName = "personal_number", colType = TextT }
      , tblColumn { colName = "company_position", colType = TextT }
      , tblColumn { colName = "phone", colType = TextT }
      , tblColumn { colName = "email", colType = TextT }
      , tblColumn { colName = "lang", colType = SmallIntT }
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
      , Check "check_mandatory_fields_unless_deleted"
              checkMandatoryFieldsUnlessDetected23
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
