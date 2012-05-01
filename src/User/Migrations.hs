module User.Migrations where

import DB
import Misc
import User.Region
import User.Tables

addUserCustomFooter :: MonadDB m => Migration m
addUserCustomFooter =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw "ALTER TABLE users ADD COLUMN customfooter TEXT"
      _ <- kRun $ SQL "UPDATE users SET customfooter = ?" [SqlNull]
      return ()
  }

removeSystemServer :: MonadDB m => Migration m
removeSystemServer =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "ALTER TABLE users DROP COLUMN system_server CASCADE"
  }

addRegionToUserSettings :: MonadDB m => Migration m
addRegionToUserSettings =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE users ADD COLUMN region SMALLINT"
      _ <- kRun $ SQL "UPDATE users SET region = ?" [toSql (defaultValue :: Region)]
      kRunRaw "ALTER TABLE users ALTER COLUMN region SET NOT NULL"
  }

addIdSerialOnUsers :: MonadDB m => Migration m
addIdSerialOnUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 4
  , mgrDo = do
      _ <- kRunRaw $ "CREATE SEQUENCE users_id_seq"
      _ <- kRunRaw $ "SELECT setval('users_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM users))"
      _ <- kRunRaw $ "ALTER TABLE users ALTER id SET DEFAULT nextval('users_id_seq')"
      return ()
  }

addCompanyNameNumberOnUsers :: MonadDB m => Migration m
addCompanyNameNumberOnUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 5
  , mgrDo = do
      _ <- kRunRaw $ "ALTER TABLE users ADD COLUMN company_name   TEXT NOT NULL DEFAULT ''"
      _ <- kRunRaw $ "ALTER TABLE users ADD COLUMN company_number TEXT NOT NULL DEFAULT ''"
      return ()
  }

addCheckLowercaseEmailsUsers :: MonadDB m => Migration m
addCheckLowercaseEmailsUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 6
  , mgrDo = do
      _ <- kRunRaw $ "UPDATE users SET email = lower(email)"
      _ <- kRunRaw $ "ALTER TABLE users ADD CONSTRAINT users_email_lowercase_chk CHECK (email = lower(email))"
      return ()
  }