module File.Tables where

import DB

tableFiles :: Table
tableFiles = tblTable
  { tblName       = "files"
  , tblVersion    = 10
  , tblColumns    =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "amazon_url", colType = TextT }
    , tblColumn { colName = "size", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "checksum", colType = BinaryT, colNullable = False }
    , tblColumn { colName = "aes_key", colType = BinaryT }
    , tblColumn { colName = "aes_iv", colType = BinaryT }
    , tblColumn { colName = "purged_time", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableFilePurgeConsumers :: Table
tableFilePurgeConsumers = tblTable
  { tblName       = "file_purge_consumers"
  , tblVersion    = 1
  , tblColumns = [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                 , tblColumn { colName = "name", colType = TextT, colNullable = False }
                 , tblColumn { colName     = "last_activity"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
                 ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableFilePurgeJobs :: Table
tableFilePurgeJobs = tblTable
  { tblName        = "file_purge_jobs"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [ (fkOnColumn "id" "files" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "reserved_by" "file_purge_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                             }
    ]
  }
