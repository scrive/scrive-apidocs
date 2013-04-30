module SMS.Tables(
    messengerTables
  , tableSMSes
  , tableSMSEvents) where

import DB

messengerTables :: [Table]
messengerTables = [
    tableSMSes
  , tableSMSEvents
  ]

tableSMSes :: Table
tableSMSes = tblTable {
    tblName = "smses"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",           SqlColDesc {colType = SqlBigIntT,            colNullable = Just False})
       , ("originator",   SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("msisdn",       SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("body",         SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("to_be_sent",   SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("sent",         SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True })
       , ("data",         SqlColDesc {colType = SqlVarCharT,           colNullable = Just False })
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE smses"
          <> "( id           BIGSERIAL     NOT NULL"
          <> ", originator   TEXT          NOT NULL"
          <> ", msisdn       TEXT          NOT NULL"
          <> ", body         TEXT          NOT NULL"
          <> ", to_be_sent   TIMESTAMPTZ   NOT NULL"
          <> ", sent         TIMESTAMPTZ       NULL"
          <> ", data         TEXT          NOT NULL"
          <> ", CONSTRAINT pk_smses PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys =  [ tblForeignKeyColumn "signatory_link_id" "signatory_links" "id"
                      ]
  }

tableSMSEvents :: Table
tableSMSEvents = tblTable {
    tblName = "sms_events"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",         SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sms_id",     SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("event",      SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("event_read", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE sms_events ("
          <> "  id           BIGSERIAL      NOT NULL"
          <> ", sms_id       BIGINT         NOT NULL"
          <> ", event        TEXT           NOT NULL"
          <> ", event_read   TIMESTAMPTZ        NULL"
          <> ", CONSTRAINT pk_sms_events PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "sms_id" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "sms_id" "smses" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }
