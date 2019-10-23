module Doc.Sealing.Tables where

import DB

tableDocumentSealingConsumers :: Table
tableDocumentSealingConsumers = tblTable
  { tblName       = "document_sealing_consumers"
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

tableDocumentSealingJobs :: Table
tableDocumentSealingJobs = tblTable
  { tblName        = "document_sealing_jobs"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "branded_domain_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [ (fkOnColumn "id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "reserved_by" "document_sealing_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                                   }
    , (fkOnColumn "branded_domain_id" "branded_domains" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  }
