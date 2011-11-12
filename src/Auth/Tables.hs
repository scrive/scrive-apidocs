module Auth.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableAccessToken :: Table
tableAccessToken = Table {
  tblName = "auth_access_token"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("access_token", SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("user_id",      SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("api_token",    SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("expires",      SqlColDesc { colType     = SqlTimestampWithZoneT
                                   , colNullable = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE auth_access_token ("
          ++ "  access_token BIGINT      NOT NULL"          
          ++ ", user_id      BIGINT      NOT NULL"
          ++ ", api_token    BIGINT      NOT NULL"          
          ++ ", expires      TIMESTAMPTZ NOT NULL"
          ++ ", CONSTRAINT pk_auth_access_token PRIMARY KEY (access_token)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "ALTER TABLE auth_access_token"
      ++ " ADD CONSTRAINT fk_auth_access_token_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE auth_access_token"
      ++ " ADD CONSTRAINT fk_auth_access_token_api_token FOREIGN KEY(api_token)"
      ++ " REFERENCES auth_api_token(api_token) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE auth_access_token"
      ++ " ADD CONSTRAINT fk_auth_access_token_unique UNIQUE (user_id, api_token)"
  }

tableAPIToken :: Table
tableAPIToken = Table {
  tblName = "auth_api_token"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("api_token",  SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("user_id",    SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("api_secret", SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("status",     SqlColDesc { colType     = SqlSmallIntT
                                 , colNullable = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE auth_api_token ("
          ++ "  api_token  BIGINT      NOT NULL"          
          ++ ", user_id    BIGINT      NOT NULL"
          ++ ", api_secret BIGINT      NOT NULL"          
          ++ ", status     SMALLINT    NOT NULL"
          ++ ", CONSTRAINT pk_auth_api_token PRIMARY KEY (api_token)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "ALTER TABLE auth_api_token"
      ++ " ADD CONSTRAINT fk_auth_api_token_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE auth_api_token"
      ++ " ADD CONSTRAINT fk_auth_api_token_unique UNIQUE (user_id, api_token)"
  }

tableAuthorization :: Table
tableAuthorization = Table {
  tblName = "auth_authorization"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("user_id",      SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("api_token",    SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("privilege",    SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE auth_authorization ("
          ++ "  user_id      BIGINT      NOT NULL"
          ++ ", api_token    BIGINT      NOT NULL"          
          ++ ", privilege    SMALLINT    NOT NULL"
          ++ ", CONSTRAINT pk_auth_access_token PRIMARY KEY (user_id, api_token, privilege)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "ALTER TABLE auth_authorization"
      ++ " ADD CONSTRAINT fk_auth_authorization_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE auth_authorization"
      ++ " ADD CONSTRAINT fk_auth_authorization_api_token FOREIGN KEY(api_token)"
      ++ " REFERENCES auth_api_token(api_token) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
