module OAuth.Migrations where

import Data.Monoid

import DB
import OAuth.Tables

tempCredentialChangeVarcharColumnsToText :: MonadDB m => Migration m
tempCredentialChangeVarcharColumnsToText =
  Migration {
    mgrTable = tableTempCredential
  , mgrFrom = 3
  , mgrDo = runSQL_ "ALTER TABLE oauth_temp_credential ALTER COLUMN callback TYPE TEXT"
  }

migrateTempCredentialRemoveEmail ::  MonadDB m => Migration m
migrateTempCredentialRemoveEmail = Migration {
  mgrTable = tableTempCredential
, mgrFrom = 2
, mgrDo = do
  runSQL_ "ALTER TABLE oauth_temp_credential DROP COLUMN email"
  runSQL_ "ALTER TABLE oauth_temp_credential ADD COLUMN user_id BIGINT NULL"
  runSQL_ $ "ALTER TABLE oauth_temp_credential"
    <> " ADD CONSTRAINT fk_oauth_temp_credential_user_id FOREIGN KEY(user_id)"
    -- we want the temp credentials to disappear when the api_token disappears
    <> " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
    <> " DEFERRABLE INITIALLY IMMEDIATE"
}
