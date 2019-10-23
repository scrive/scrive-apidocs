module Doc.Signing.Tables where

import DB

tableDocumentSigningConsumers :: Table
tableDocumentSigningConsumers = tblTable
  { tblName       = "document_signing_consumers"
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

tableDocumentSigningJobs :: Table
tableDocumentSigningJobs = tblTable
  { tblName        = "document_signing_jobs"
  , tblVersion     = 5
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName     = "cancelled"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "branded_domain_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "client_ip_v4", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "client_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "client_name", colType = TextT }
    , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "fields", colType = JsonT, colNullable = False }
    , tblColumn { colName     = "accepted_attachments"
                , colType     = ArrayT BigIntT
                , colNullable = False
                }
    , tblColumn { colName = "screenshots", colType = JsonT, colNullable = False }
    , tblColumn { colName = "last_check_status", colType = TextT }
    , tblColumn { colName     = "not_uploaded_sig_attachments"
                , colType     = ArrayT TextT
                , colNullable = False
                , colDefault  = Just "'{}'::text[]"
                }
    , tblColumn { colName     = "signature_provider"
                , colType     = SmallIntT
                , colNullable = False
                , colDefault  = Just "5"
                } -- CgiGrpBankID
    , tblColumn { colName     = "consent_responses"
                , colType     = JsonT
                , colNullable = False
                , colDefault  = Just "'[]'::json"
                }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [ (fkOnColumn "id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "reserved_by" "document_signing_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                                   }
    , (fkOnColumn "branded_domain_id" "branded_domains" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  }
