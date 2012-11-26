module User.Tables where

import DB

tableUsers :: Table
tableUsers = tblTable {
    tblName = "users"
  , tblVersion = 11
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("password", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("salt", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("is_company_admin", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("account_suspended", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("has_accepted_terms_of_service", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("signup_method", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("company_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("first_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("last_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("personal_number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("company_position", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("phone", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("mobile", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("email", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("lang", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("customfooter", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("company_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("company_number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("is_free", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE users ("
          <> "  id BIGINT NOT NULL"
          <> ", password BYTEA NULL"
          <> ", salt BYTEA NULL"
          <> ", is_company_admin BOOL NOT NULL"
          <> ", account_suspended BOOL NOT NULL"
          <> ", has_accepted_terms_of_service TIMESTAMPTZ NULL"
          <> ", signup_method SMALLINT NOT NULL"
          <> ", company_id BIGINT NULL"
          <> ", first_name TEXT NOT NULL"
          <> ", last_name TEXT NOT NULL"
          <> ", personal_number TEXT NOT NULL"
          <> ", company_position TEXT NOT NULL"
          <> ", phone TEXT NOT NULL"
          <> ", mobile TEXT NOT NULL"
          <> ", email TEXT NOT NULL"
          <> ", lang SMALLINT NOT NULL"
          <> ", deleted BOOL NOT NULL"
          <> ", customfooter TEXT NULL"
          <> ", company_name   TEXT NOT NULL"
          <> ", company_number TEXT NOT NULL"
          <> ", is_free BOOL NOT NULL DEFAULT FALSE"
          <> ", CONSTRAINT pk_users PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "company_id"
                 , tblIndexOnColumn "email"
                 ]
  , tblForeignKeys = [ (tblForeignKeyColumn "company_id" "companies" "id") ]
  , tblPutProperties = do
    kRunRaw $ "CREATE SEQUENCE users_id_seq"
    kRunRaw $ "SELECT setval('users_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM users))"
    kRunRaw $ "ALTER TABLE users ALTER id SET DEFAULT nextval('users_id_seq')"
    kRunRaw $ "ALTER TABLE users ADD CONSTRAINT users_email_lowercase_chk CHECK (email = lower(email))"
    return ()
  }

tableUserInviteInfos :: Table
tableUserInviteInfos = tblTable {
    tblName = "user_invite_infos"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> do
    case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("inviter_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("invite_time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("invite_type", SqlColDesc {colType = SqlSmallIntT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE user_invite_infos ("
          <> "  user_id BIGINT NOT NULL"
          <> ", inviter_id BIGINT NOT NULL"
          <> ", invite_time TIMESTAMPTZ NULL"
          <> ", invite_type SMALLINT NULL"
          <> ", CONSTRAINT pk_user_invite_infos PRIMARY KEY (user_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "inviter_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }
