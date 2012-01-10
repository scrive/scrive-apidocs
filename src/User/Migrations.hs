module User.Migrations where

import Database.HDBC

import DB.Classes
import DB.Model
import Misc
import User.Region
import User.Tables

addUserCustomFooter :: Migration
addUserCustomFooter =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 3
  , mgrDo = wrapDB $ \conn -> do
      _ <- run conn "ALTER TABLE users ADD COLUMN customfooter TEXT" []
      _ <- run conn "UPDATE users SET customfooter = ?" [toSql (Nothing :: Maybe String)]
      return ()
  }

removeSystemServer :: Migration
removeSystemServer =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 2
  , mgrDo = wrapDB $ \conn -> do
      _ <- run conn "ALTER TABLE users DROP COLUMN system_server CASCADE" []
      return ()
  }

addRegionToUserSettings :: Migration
addRegionToUserSettings =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 1
  , mgrDo = wrapDB $ \conn -> do
      _ <- run conn "ALTER TABLE users ADD COLUMN region SMALLINT" []
      _ <- run conn "UPDATE users SET region = ?" [toSql (defaultValue :: Region)]
      _ <- run conn "ALTER TABLE users ALTER COLUMN region SET NOT NULL" []
      return ()
  }
