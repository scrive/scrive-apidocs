module User.CallbackScheme.Migrations where

import DB

migrateOAuth2CallbackSchemeToHi3GCallbackScheme :: MonadDB m => Migration m
migrateOAuth2CallbackSchemeToHi3GCallbackScheme = Migration
  { mgrTableName = "user_callback_scheme"
  , mgrFrom      = 2
  , mgrAction    =
    StandardMigration $ do
      runSQL_
        "UPDATE user_callback_scheme SET callback_scheme = REPLACE(callback_scheme,'OAuth2Scheme','Hi3GScheme')"
  }

