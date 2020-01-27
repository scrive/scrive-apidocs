module Doc.Extending.Tables where

import DB

tableDocumentExtendingConsumers :: Table
tableDocumentExtendingConsumers = tblTable
  { tblName       = "document_extending_consumers"
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

tableDocumentExtendingJobs :: Table
tableDocumentExtendingJobs = tblTable
  { tblName        = "document_extending_jobs"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys = [ (fkOnColumn "id" "documents" "id") { fkOnDelete = ForeignKeyCascade
                                                          }
                     , (fkOnColumn "reserved_by" "document_extending_consumers" "id")
                       { fkOnDelete = ForeignKeySetNull
                       }
                     ]
  , tblIndexes     =
    [ indexOnColumn "run_at"
    , (indexOnColumn "attempts") { idxWhere = Just "attempts > 1 AND finished_at IS NULL"
                                 }
    ]
  }
