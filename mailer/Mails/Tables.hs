module Mails.Tables where

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
       , ("from", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("to", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("content", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("sent", SqlColDesc { colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("sendgrid_event", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE mails ("
          ++ "  id BIGSERIAL NOT NULL"
          ++ ", from TEXT NOT NULL"
          ++ ", to TEXT NOT NULL"
          ++ ", content TEXT NULL"
          ++ ", sent TIMESTAMPTZ NULL"
          ++ ", sendgrid_event TEXT NULL"
          ++ ", CONSTRAINT pk_mails PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = return ()
  }
