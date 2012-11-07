{-# LANGUAGE OverloadedStrings #-}
module PadQueue.Tables (tablePadQueue) where

import DB

tablePadQueue :: Table
tablePadQueue = Table {
    tblName = "padqueue"
  , tblVersion = 4
  , tblCreateOrValidate = \desc -> case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("signatorylink_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE padqueue ("
          ++ "  user_id BIGINT NOT NULL"
          ++ ", document_id BIGINT NOT NULL"
          ++ ", signatorylink_id BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_padqueue PRIMARY KEY (user_id)"

          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE padqueue"
      ++ " ADD CONSTRAINT fk_padqueue_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE padqueue"
      ++ " ADD CONSTRAINT fk_padqueue_documents_signatory FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE padqueue"
      ++ " ADD CONSTRAINT fk_padqueue_signatorylinks FOREIGN KEY(signatorylink_id)"
      ++ " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    return ()
  }
