module OAuth.Tables where

import DB

{- |
   Module: OAuth.Tables
   Author: Eric Normand

   Description:

     OAuth is a widely accepted web standard. The spec is available at
     http://tools.ietf.org/html/rfc5849 . More information is available at
     http://oauth.net/ . There are many libraries for accessing an OAuth
     enabled service from different languages. This should allow an easy
     onramp for using the API.

     OAuth is a mechanism for a service to control a user's account without
     the user sharing their username and password. Instead, the user is guided
     through a flow where he/she grants permissions to the service.

     These tables manage that flow.
-}

{- |
   A User may have one or more API Tokens, which allow it to request privileges from other Users.

   These API tokens are used in the first step of the OAuth permission flow.

   Users may add and delete the API Tokens in their account screen, though most users will not 
   use them.
-}
tableAPIToken :: Table
tableAPIToken = tblTable {
  tblName = "oauth_api_token"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "api_token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "api_secret", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

{-
    Access Tokens belong to an API Token and grant privileges from a User.

    Access Tokens are generated in the last step of the OAuth flow. They are
    linked to a User account and to an API Token. This signifies that the owner
    of the API Token has been granted privileges by the User refered to by
    user_id.
-}
tableAccessToken :: Table
tableAccessToken = tblTable {
  tblName = "oauth_access_token"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "access_token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "access_secret", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "api_token_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "created", colType = TimestampWithZoneT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "api_token_id" "oauth_api_token" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  }

{-
    Privileges granted to an Access token by a User.

    These privileges belong to an Access Token.
-}
tablePrivilege :: Table
tablePrivilege = tblTable {
  tblName = "oauth_privilege"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "access_token_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "privilege", colType = SmallIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["access_token_id", "privilege"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "access_token_id" "oauth_access_token" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
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
        <> " ADD CONSTRAINT fk_oauth_temp_credential_user_id FOREIGN KEY(user_id)"
        -- we want the temp credentials to disappear when the api_token disappears
        <> " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
        <> " DEFERRABLE INITIALLY IMMEDIATE"

  }

{- |
   Temporary Credentials are used during the OAuth flow.

   A Temporary Credential is created at the beginning of the OAuth flow
   and represents a request for privileges.

   They should expire after 10 minutes and only be used once.
-}
tableTempCredential :: Table
tableTempCredential = tblTable {
  tblName = "oauth_temp_credential"
  , tblVersion = 3
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "temp_token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "temp_secret", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "api_token_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "verifier", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "callback", colType = TextT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "api_token_id" "oauth_api_token" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    , (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

{- |
   Temporary Privileges are the privileges that are being requested from
   the User.

   They belong to a Temp Credential.
-}
tableTempPrivileges :: Table
tableTempPrivileges = tblTable {
  tblName = "oauth_temp_privileges"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "temp_token_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "privilege", colType = SmallIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["temp_token_id", "privilege"]
  , tblForeignKeys = [
    (tblForeignKeyColumn "temp_token_id" "oauth_temp_credential" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  }
