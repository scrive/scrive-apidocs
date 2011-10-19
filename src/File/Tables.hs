module File.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableFiles :: Table
tableFiles = Table {
    tblName = "files"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("content", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("amazon_bucket", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("amazon_url", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("disk_path", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE files ("
          ++ "  id BIGINT NOT NULL"
          ++ ", name TEXT NOT NULL"
          ++ ", content BYTEA NULL"
          ++ ", amazon_bucket TEXT NULL"
          ++ ", amazon_url TEXT NULL"
          ++ ", disk_path TEXT NULL"
          ++ ", CONSTRAINT pk_files PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \_conn -> do
    {-
    runRaw conn $ "ALTER TABLE user_friends"
      ++ " ADD CONSTRAINT fk_user_friends_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE user_friends"
      ++ " ADD CONSTRAINT fk_user_friends_users_2 FOREIGN KEY(friend_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    -}
    return ()
  }
