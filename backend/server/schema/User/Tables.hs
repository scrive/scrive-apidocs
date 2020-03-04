module User.Tables where

import DB

tableUsers :: Table
tableUsers = tblTable
  { tblName        = "users"
  , tblVersion     = 30
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "password", colType = BinaryT }
    , tblColumn { colName = "salt", colType = BinaryT }
    , tblColumn { colName = "is_company_admin", colType = BoolT, colNullable = False }
    , tblColumn { colName = "account_suspended", colType = BoolT, colNullable = False }
    , tblColumn { colName = "has_accepted_terms_of_service"
                , colType = TimestampWithZoneT
                }
    , tblColumn { colName = "signup_method", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "first_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "last_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "personal_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "company_position", colType = TextT, colNullable = False }
    , tblColumn { colName = "phone", colType = TextT, colNullable = False }
    , tblColumn { colName = "email", colType = TextT, colNullable = False }
    , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    , tblColumn { colName     = "associated_domain_id"
                , colType     = BigIntT
                , colNullable = False
                }
    , tblColumn { colName     = "password_algorithm"
                , colType     = SmallIntT
                , colNullable = True
                }
    , tblColumn { colName = "totp_key", colType = BinaryT, colNullable = True }
    , tblColumn { colName     = "totp_active"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "idle_doc_timeout_preparation", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_closed", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_canceled", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_timedout", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_rejected", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_error", colType = SmallIntT }
    , tblColumn { colName     = "immediate_trash"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "home_folder_id", colType = BigIntT, colNullable = True }
    , tblColumn { colName     = "totp_is_mandatory"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "sysauth"
                , colType     = SmallIntT
                , colNullable = False
                , colDefault  = Just "1"
                }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblChecks      = [ tblCheck { chkName      = "check_users_lowercase_email"
                                , chkCondition = "email = lower(email)"
                                }
                     ]
  , tblForeignKeys = [ fkOnColumn "user_group_id"        "user_groups"     "id"
                     , fkOnColumn "home_folder_id"       "folders"         "id"
                     , fkOnColumn "associated_domain_id" "branded_domains" "id"
                     ]
  , tblIndexes     =
    [ indexOnColumn "user_group_id"
    , indexOnColumn "home_folder_id"
    , (indexOnColumn "email") { idxUnique = True, idxWhere = Just ("deleted IS NULL") }
    ]
  }

tableTemporaryLoginTokens :: Table
tableTemporaryLoginTokens = tblTable
  { tblName        = "temporary_login_tokens"
  , tblVersion     = 1
  , tblColumns = [ tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
                 , tblColumn { colName     = "user_id"
                             , colType     = BigIntT
                             , colNullable = False
                             }
                 , tblColumn { colName     = "expiration_time"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
                 ]
  , tblPrimaryKey  = pkOnColumn "hash"
  , tblForeignKeys =
    [(fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }]
  }


---------------------------------

tableUserTags :: Table
tableUserTags = tblTable
  { tblName        = "user_tags"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "value", colType = TextT, colNullable = False }
    , tblColumn { colName = "internal", colType = BoolT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumns ["user_id", "name", "internal"]
  , tblIndexes     = [indexOnColumns ["value", "internal"]]
  , tblForeignKeys =
    [(fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }]
  }
