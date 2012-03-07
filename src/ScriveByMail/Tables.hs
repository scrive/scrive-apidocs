module ScriveByMail.Tables 
       (
         tableMailAPIDelay
       )
    where

import Database.HDBC

import DB.Classes
import DB.Model

tableMailAPIDelay :: Table
tableMailAPIDelay = Table {
    tblName = "mail_api_delay"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",             SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("key",            SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("email",          SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("email_text",     SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("time",           SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("expires",        SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE mail_api_delay ("
          ++ "  id          BIGSERIAL NOT NULL"
          ++ ", key            BIGINT NOT NULL"
          ++ ", email            TEXT NOT NULL"
          ++ ", email_text    INTEGER NOT NULL"
          ++ ", time      TIMESTAMPTZ NOT NULL"
          ++ ", expires   TIMESTAMPTZ NOT NULL"
          ++ ", CONSTRAINT pk_mail_api_delay PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = return ()
  }
