module SMS.Tables where

import DB

messengerTables :: [Table]
messengerTables = [
    tableSMSes
  ]

tableSMSes :: Table
tableSMSes = tblTable {
    tblName = "smses"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",           SqlColDesc {colType = SqlBigIntT,            colNullable = Just False})
       , ("token",        SqlColDesc {colType = SqlBigIntT,            colNullable = Just False})
       , ("originator",   SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("msisdn",       SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("body",         SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("to_be_sent",   SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("sent",         SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True })
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE smses"
          <> "( id           BIGSERIAL     NOT NULL"
          <> ", token        BIGINT        NOT NULL"
          <> ", originator   TEXT          NOT NULL"
          <> ", msisdn       TEXT          NOT NULL"
          <> ", body         TEXT          NOT NULL"
          <> ", to_be_sent   TIMESTAMPTZ   NOT NULL"
          <> ", sent         TIMESTAMPTZ       NULL"
          <> ", CONSTRAINT pk_smses PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  }
