module OAuth.Migrations where

import DB
import OAuth.Tables

migrateTempCredentialRemoveEmail ::  MonadDB m => Migration m
migrateTempCredentialRemoveEmail = Migration {
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
