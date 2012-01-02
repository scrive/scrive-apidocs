module Mails.Tables (
    tableMails
  ) where

import Database.HDBC

import DB.Classes
import DB.Model

tableMails :: Table
tableMails = Table {
    tblName = "mails"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sender", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("receivers", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("title", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("content", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("attachments", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("x_smtpapi", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("sent", SqlColDesc { colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("event", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("event_read", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE mails ("
          ++ "  id BIGSERIAL NOT NULL"
          ++ ", sender TEXT NOT NULL"
          ++ ", receivers TEXT NOT NULL"
          ++ ", title TEXT NULL"
          ++ ", content TEXT NULL"
          ++ ", attachments TEXT NULL"
          ++ ", x_smtpapi TEXT NULL"
          ++ ", sent TIMESTAMPTZ NULL"
          ++ ", event TEXT NULL"
          ++ ", event_read TIMESTAMPTZ NULL"
          ++ ", CONSTRAINT pk_mails PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = return ()
  }
