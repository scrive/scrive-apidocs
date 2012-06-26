module File.Tables where

import DB

tableFiles :: Table
tableFiles = Table {
    tblName = "files"
  , tblVersion = 3
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("content", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("amazon_bucket", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("amazon_url", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("disk_path", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("checksum", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("aes_key", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("aes_iv", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE files ("
          ++ "  id BIGINT NOT NULL"
          ++ ", name TEXT NOT NULL"
          ++ ", content BYTEA NULL"
          ++ ", amazon_bucket TEXT NULL"
          ++ ", amazon_url TEXT NULL"
          ++ ", disk_path TEXT NULL"
          ++ ", checksum BYTEA NULL"
          ++ ", aes_key BYTEA NULL"
          ++ ", aes_iv BYTEA NULL"
          ++ ", CONSTRAINT pk_files PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE files_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      _ <- kRunRaw $ "SELECT setval('files_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM files))"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE files ALTER id SET DEFAULT nextval('files_id_seq')"
      return ()
  }
