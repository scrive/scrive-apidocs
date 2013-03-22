module HostClock.Tables where

import DB

tableHostClock :: Table
tableHostClock = tblTable {
    tblName = "host_clock"
  , tblVersion = 0
  , tblCreateOrValidate = \desc -> case desc of
      [  ("time",            SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("clock_offset",    SqlColDesc {colType = SqlFloatT,             colNullable = Just True})
       , ("clock_frequency", SqlColDesc {colType = SqlFloatT,             colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE host_clock"
          <> "( time            TIMESTAMPTZ      NOT NULL" -- time when data was collected
          <> ", clock_offset    DOUBLE PRECISION     NULL" -- as reported by ntpdate: difference between host clock and reference servers; NULL if ntpdate failed
          <> ", clock_frequency DOUBLE PRECISION NOT NULL" -- (relative) kernel phase-lock loop frequency
          <> ", CONSTRAINT pk_host_clock PRIMARY KEY (time)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  }
