module User.Tables where

import DB

tableUsers :: Table
tableUsers = tblTable {
    tblName = "users"
  , tblVersion = 14
    , tblColumns = [
      tblColumn { colName = "id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "password", colType = BinaryT }
    , tblColumn { colName = "salt", colType = BinaryT }
    , tblColumn { colName = "is_company_admin", colType = BoolT, colNullable = False }
    , tblColumn { colName = "account_suspended", colType = BoolT, colNullable = False }
    , tblColumn { colName = "has_accepted_terms_of_service", colType = TimestampWithZoneT }
    , tblColumn { colName = "signup_method", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "company_id", colType = BigIntT }
    , tblColumn { colName = "first_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "last_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "personal_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "company_position", colType = TextT, colNullable = False }
    , tblColumn { colName = "phone", colType = TextT, colNullable = False }
    , tblColumn { colName = "email", colType = TextT, colNullable = False }
    , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "deleted", colType = BoolT, colNullable = False }
    , tblColumn { colName = "company_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "company_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "is_free", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "associated_domain", colType = TextT }
    ]
  , tblPrimaryKey = ["id"]
  , tblChecks = [TableCheck "lowercase_email" "email = lower(email)"]
  , tblForeignKeys = [tblForeignKeyColumn "company_id" "companies" "id"]
  , tblIndexes = [
      tblIndexOnColumn "company_id"
    , tblIndexOnColumn "email"
    ]
  , tblPutProperties = do
    kRunRaw "CREATE SEQUENCE users_id_seq"
    kRunRaw "SELECT setval('users_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM users))"
    kRunRaw "ALTER TABLE users ALTER id SET DEFAULT nextval('users_id_seq')"
  }

tableUserInviteInfos :: Table
tableUserInviteInfos = tblTable {
    tblName = "user_invite_infos"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "inviter_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "invite_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "invite_type", colType = SmallIntT }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "inviter_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
