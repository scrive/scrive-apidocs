module User.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableUserFriends :: Table
tableUserFriends = Table {
    tblName = "user_friends"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("friend_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE user_friends ("
          ++ "  user_id BIGINT NOT NULL"
          ++ ", friend_id BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_user_friends PRIMARY KEY (user_id, friend_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE user_friends"
      ++ " ADD CONSTRAINT fk_user_friends_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE user_friends"
      ++ " ADD CONSTRAINT fk_user_friends_users_2 FOREIGN KEY(friend_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableUsers :: Table
tableUsers = Table {
    tblName = "users"
  , tblVersion = 6
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("password", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("salt", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("is_company_admin", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("account_suspended", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("has_accepted_terms_of_service", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("signup_method", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("service_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("company_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("first_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("last_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("personal_number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("company_position", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("phone", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("mobile", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("email", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("preferred_design_mode", SqlColDesc {colType = SqlSmallIntT, colNullable = Just True})
       , ("lang", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("deleted", SqlColDesc {colType = SqlBitT, colNullable = Just False})
       , ("region", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("customfooter", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("company_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("company_number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE users ("
          ++ "  id BIGINT NOT NULL"
          ++ ", password BYTEA NULL"
          ++ ", salt BYTEA NULL"
          ++ ", is_company_admin BOOL NOT NULL"
          ++ ", account_suspended BOOL NOT NULL"
          ++ ", has_accepted_terms_of_service TIMESTAMPTZ NULL"
          ++ ", signup_method SMALLINT NOT NULL"
          ++ ", service_id TEXT NULL"
          ++ ", company_id BIGINT NULL"
          ++ ", first_name TEXT NOT NULL"
          ++ ", last_name TEXT NOT NULL"
          ++ ", personal_number TEXT NOT NULL"
          ++ ", company_position TEXT NOT NULL"
          ++ ", phone TEXT NOT NULL"
          ++ ", mobile TEXT NOT NULL"
          ++ ", email TEXT NOT NULL"
          ++ ", preferred_design_mode SMALLINT NULL"
          ++ ", lang SMALLINT NOT NULL"
          ++ ", deleted BOOL NOT NULL"
          ++ ", region SMALLINT NOT NULL"
          ++ ", customfooter TEXT NULL"
          ++ ", company_name   TEXT NOT NULL"
          ++ ", company_number TEXT NOT NULL"
          ++ ", CONSTRAINT pk_users PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw "CREATE INDEX idx_users_service_id ON users(service_id)"
    kRunRaw "CREATE INDEX idx_users_company_id ON users(company_id)"
    kRunRaw "CREATE INDEX idx_users_email ON users(email)"
    kRunRaw $ "ALTER TABLE users"
      ++ " ADD CONSTRAINT fk_users_services FOREIGN KEY(service_id)"
      ++ " REFERENCES services(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE users"
      ++ " ADD CONSTRAINT fk_users_companies FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "CREATE SEQUENCE users_id_seq"
    kRunRaw $ "SELECT setval('users_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM users))"
    kRunRaw $ "ALTER TABLE users ALTER id SET DEFAULT nextval('users_id_seq')"
    return ()
  }

tableUserMailAPIs :: Table
tableUserMailAPIs = Table {
    tblName = "user_mail_apis"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("key", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("daily_limit", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sent_today", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("last_sent_date", SqlColDesc {colType = SqlDateT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE user_mail_apis ("
          ++ "  user_id BIGINT NOT NULL"
          ++ ", key BIGINT NOT NULL"
          ++ ", daily_limit INTEGER NOT NULL"
          ++ ", sent_today INTEGER NOT NULL"
          ++ ", last_sent_date DATE NOT NULL"
          ++ ", CONSTRAINT pk_user_mail_apis PRIMARY KEY (user_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE user_mail_apis"
      ++ " ADD CONSTRAINT fk_user_mail_apis_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableUserInviteInfos :: Table
tableUserInviteInfos = Table {
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
          ++ "  user_id BIGINT NOT NULL"
          ++ ", inviter_id BIGINT NOT NULL"
          ++ ", invite_time TIMESTAMPTZ NULL"
          ++ ", invite_type SMALLINT NULL"
          ++ ", CONSTRAINT pk_user_invite_infos PRIMARY KEY (user_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE user_invite_infos"
      ++ " ADD CONSTRAINT fk_user_invite_info_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE user_invite_infos"
      ++ " ADD CONSTRAINT fk_user_invite_infos_users FOREIGN KEY(inviter_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
