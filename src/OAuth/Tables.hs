module OAuth.Tables where

import Database.HDBC

import DB

{-
   A User may have one or more API Tokens, which allow it to request privileges from other Users.
-}
tableAPIToken :: Table
tableAPIToken = Table {
  tblName = "oauth_api_token"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("id",         SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("api_token",  SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("api_secret", SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("user_id",    SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False})] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE oauth_api_token ("
          ++ "  id         BIGSERIAL   NOT NULL"
          ++ ", api_token  BIGINT      NOT NULL"          
          ++ ", api_secret BIGINT      NOT NULL"          
          ++ ", user_id    BIGINT      NOT NULL"
          ++ ", CONSTRAINT pk_oauth_api_token PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE oauth_api_token"
      ++ " ADD CONSTRAINT fk_oauth_api_token_users FOREIGN KEY(user_id)"
      -- we want the api tokens to disappear when the User disappears
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

{-
    Access Tokens belong to an API Token and grant privileges from a User.

-}
tableAccessToken :: Table
tableAccessToken = Table {
  tblName = "oauth_access_token"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("id", SqlColDesc { colType = SqlBigIntT
                         , colNullable = Just False}),
       ("access_token",  SqlColDesc { colType     = SqlBigIntT
                                    , colNullable = Just False}),
       ("access_secret", SqlColDesc { colType     = SqlBigIntT
                                    , colNullable = Just False}),
       ("api_token_id",  SqlColDesc { colType     = SqlBigIntT
                                    , colNullable = Just False}),
       ("user_id",       SqlColDesc { colType     = SqlBigIntT
                                    , colNullable = Just False}),
       ("created",       SqlColDesc { colType     = SqlTimestampWithZoneT
                                    , colNullable = Just False})] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE oauth_access_token ("
          ++ "  id            BIGSERIAL   NOT NULL"
          ++ ", access_token  BIGINT      NOT NULL" 
          ++ ", access_secret BIGINT      NOT NULL" 
          ++ ", api_token_id  BIGINT      NOT NULL"
          -- UserID of the resource owner          
          ++ ", user_id       BIGINT      NOT NULL"
          -- the creation date
          ++ ", created       TIMESTAMPTZ NOT NULL"
          ++ ", CONSTRAINT pk_oauth_access_token PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE oauth_access_token"
      ++ " ADD CONSTRAINT fk_oauth_access_token_users FOREIGN KEY(user_id)"
      -- remove the Access Token if we delete the user
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE oauth_access_token"
      ++ " ADD CONSTRAINT fk_oauth_access_token_api_token FOREIGN KEY(api_token_id)"
      -- also if we delete the api_token, we delete the Access Token
      ++ " REFERENCES oauth_api_token(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

{-
    Privileges granted to an Access token by a User

-}
tablePrivilege :: Table
tablePrivilege = Table {
  tblName = "oauth_privilege"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("access_token_id",  SqlColDesc { colType     = SqlBigIntT
                                    , colNullable = Just False}),
       ("privilege",     SqlColDesc { colType     = SqlSmallIntT
                                    , colNullable = Just False})] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE oauth_privilege ("
          ++ "  access_token_id  BIGINT      NOT NULL" 
          ++ ", privilege     SMALLINT    NOT NULL"
          ++ ", CONSTRAINT pk_oauth_privilege PRIMARY KEY (access_token_id, privilege)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE oauth_privilege"
      ++ " ADD CONSTRAINT fk_oauth_privilege FOREIGN KEY(access_token_id)"
      -- remove the privilege if we delete the Access Token
      ++ " REFERENCES oauth_access_token(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

migrateTempCredentialRemoveEmail ::  MonadDB m => Migration m
migrateTempCredentialRemoveEmail = 
  Migration {
    mgrTable = tableTempCredential
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "ALTER TABLE oauth_temp_credential DROP COLUMN email"
      kRunRaw "ALTER TABLE oauth_temp_credential ADD COLUMN user_id BIGINT NULL"
      kRunRaw $ "ALTER TABLE oauth_temp_credential"
        ++ " ADD CONSTRAINT fk_oauth_temp_credential_user_id FOREIGN KEY(user_id)"
        -- we want the temp credentials to disappear when the api_token disappears
        ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
        ++ " DEFERRABLE INITIALLY IMMEDIATE"

  } 

{-
   Temporary Credentials are used during the OAuth flow.

   They should expire after 10 minutes and only be used once.
-}
tableTempCredential :: Table
tableTempCredential = Table {
  tblName = "oauth_temp_credential"
  , tblVersion = 3
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("id", SqlColDesc { colType = SqlBigIntT
                         , colNullable = Just False}),
       ("temp_token",   SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("temp_secret",  SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("api_token_id",  SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("verifier",    SqlColDesc { colType     = SqlBigIntT
                                 , colNullable = Just False}),
       ("expires",    SqlColDesc { colType     = SqlTimestampWithZoneT
                                 , colNullable = Just False}),
       ("callback",  SqlColDesc { colType = SqlVarCharT
                                , colNullable = Just False}),
       ("user_id",    SqlColDesc { colType      = SqlBigIntT
                                 , colNullable  = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE oauth_temp_credential ("
          ++ "  id          BIGSERIAL   NOT NULL"
          ++ ", temp_token  BIGINT      NOT NULL"          
          ++ ", temp_secret BIGINT      NOT NULL"          
          ++ ", api_token_id BIGINT      NOT NULL"          
          ++ ", verifier    BIGINT      NOT NULL"
          ++ ", expires     TIMESTAMPTZ NOT NULL"
          ++ ", callback    VARCHAR     NOT NULL"
          ++ ", user_id     BIGINT          NULL"
          ++ ", CONSTRAINT pk_oauth_temp_credential PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE oauth_temp_credential"
      ++ " ADD CONSTRAINT fk_oauth_temp_credential FOREIGN KEY(api_token_id)"
      -- we want the temp credentials to disappear when the api_token disappears
      ++ " REFERENCES oauth_api_token(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE oauth_temp_credential"
      ++ " ADD CONSTRAINT fk_oauth_temp_credential_user_id FOREIGN KEY(user_id)"
      -- we want the temp credentials to disappear when the api_token disappears
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableTempPrivileges :: Table
tableTempPrivileges = Table {
  tblName = "oauth_temp_privileges"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("temp_token_id",   SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("privilege", SqlColDesc { colType = SqlSmallIntT
                                , colNullable = Just False})] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE oauth_temp_privileges ("
          ++ "  temp_token_id  BIGINT      NOT NULL"          
          ++ ", privilege   SMALLINT    NOT NULL"          
          ++ ", CONSTRAINT pk_oauth_temp_privileges PRIMARY KEY (temp_token_id, privilege)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE oauth_temp_privileges"
      ++ " ADD CONSTRAINT fk_oauth_temp_privileges_temp_token FOREIGN KEY(temp_token_id)"
      -- we want the temp credentials to disappear when the api_token disappears
      ++ " REFERENCES oauth_temp_credential(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
