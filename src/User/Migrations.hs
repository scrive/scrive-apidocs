module User.Migrations where

import Database.HDBC

import DB.Classes
import DB.Model
import Misc
import User.Region
import User.Tables

addRegionToUserSettings :: Migration  
addRegionToUserSettings =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 1
  , mgrDo = wrapDB $ \conn -> do
      _ <- run conn "ALTER TABLE users ADD COLUMN region SMALLINT AFTER lang" []
      _ <- run conn "UPDATE users SET region = ?" [toSql (defaultValue :: Region)]
      _ <- run conn "ALTER TABLE users ALTER COLUMN region SET NOT NULL" []
      return ()
  }

