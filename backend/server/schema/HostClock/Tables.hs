module HostClock.Tables where

import DB

tableHostClock :: Table
tableHostClock = tblTable
  { tblName       = "host_clock"
  , tblVersion    = 0
  , tblColumns    =
    [ tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False } -- time when data was collected
    , tblColumn { colName = "clock_offset", colType = DoubleT } -- as reported by ntpdate: difference between host clock and reference servers; NULL if ntpdate failed
    , tblColumn { colName = "clock_frequency", colType = DoubleT, colNullable = False } -- (relative) kernel phase-lock loop frequency
    ]
  , tblPrimaryKey = pkOnColumn "time"
  }
