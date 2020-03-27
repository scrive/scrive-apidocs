module Mails.Tables (
    mailerComposites
  , mailerTables
  , tableMailerWorkers
  , tableMailerJobs
  , tableMails
  , tableMailEvents
  , tableMailAttachments
  , ctMailAttachment
  ) where

import DB
import MinutesTime

mailerComposites :: [CompositeType]
mailerComposites = [ctMailAttachment]

mailerTables :: [Table]
mailerTables =
  [tableMailerWorkers, tableMailerJobs, tableMails, tableMailEvents, tableMailAttachments]

----------------------------------------

tableMailerWorkers :: Table
tableMailerWorkers = tblTable
  { tblName       = "mailer_workers"
  , tblVersion    = 1
  , tblColumns = [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                 , tblColumn { colName     = "last_activity"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
                 , tblColumn { colName = "name", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableMailerJobs :: Table
tableMailerJobs = tblTable
  { tblName         = "mailer_jobs"
  , tblVersion      = 1
  , tblColumns = [ tblColumn { colName = "id", colType = TextT, colNullable = False }
                 , tblColumn { colName = "run_at", colType = TimestampWithZoneT }
                 , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
                 , tblColumn { colName = "reserved_by", colType = BigIntT }
                 , tblColumn { colName     = "attempts"
                             , colType     = IntegerT
                             , colNullable = False
                             , colDefault  = Just "0"
                             }
                 ]
  , tblPrimaryKey   = pkOnColumn "id"
  , tblForeignKeys  =
    [(fkOnColumn "reserved_by" "mailer_workers" "id") { fkOnDelete = ForeignKeySetNull }]
  , tblInitialSetup =
    Just $ TableInitialSetup
      { checkInitialSetup = return True
      , initialSetup      =
        forM_ jobs $ \values -> runQuery_ $ rawSQL
          "INSERT INTO mailer_jobs (id, run_at, finished_at) VALUES ($1, $2, $3)"
          values
      }
  }
  where
    jobs :: [(Text, Maybe UTCTime, Maybe UTCTime)]
    jobs =
      [ ("clean_old_emails"           , Just unixEpoch, Nothing)
      , ("perform_service_test"       , Just unixEpoch, Nothing)
      , ("collect_service_test_result", Nothing       , Just unixEpoch)
      ]

tableMails :: Table
tableMails = tblTable
  { tblName        = "mails"
  , tblVersion     = 8
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "sender", colType = TextT, colNullable = False }
    , tblColumn { colName = "receivers", colType = TextT, colNullable = False }
    , tblColumn { colName = "title", colType = TextT, colNullable = False }
    , tblColumn { colName = "content", colType = TextT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "service_test", colType = BoolT, colNullable = False }
    , tblColumn { colName     = "attempts"
                , colType     = IntegerT
                , colNullable = False
                , colDefault  = Just "0"
                }
    , tblColumn { colName = "reply_to", colType = TextT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [(fkOnColumn "reserved_by" "mailer_workers" "id") { fkOnDelete = ForeignKeySetNull }]
  , tblIndexes     =
    [ (indexOnColumn "run_at") { idxWhere = Just "run_at IS NOT NULL" }
    , (indexOnColumn "attempts") { idxWhere = Just "attempts > 1 AND finished_at IS NULL"
                                 }
    , (indexOnColumn "service_test") { idxWhere = Just "service_test = true" }
    ]
  }

tableMailAttachments :: Table
tableMailAttachments = tblTable
  { tblName = "mail_attachments"
  , tblVersion = 3
  , tblColumns = [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                 , tblColumn { colName     = "mail_id"
                             , colType     = BigIntT
                             , colNullable = False
                             }
                 , tblColumn { colName = "name", colType = TextT, colNullable = False }
                 , tblColumn { colName = "content", colType = BinaryT }
                 , tblColumn { colName = "file_id", colType = BigIntT }
                 ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [ (fkOnColumn "mail_id" "mails" "id") { fkOnDelete = ForeignKeyCascade
                                                           }
                     , fkOnColumn "file_id" "files" "id"
                     ]
  , tblIndexes = [indexOnColumn "mail_id", indexOnColumn "file_id"]
  }

ctMailAttachment :: CompositeType
ctMailAttachment = CompositeType
  { ctName    = "mail_attachment_c1"
  , ctColumns = [ CompositeColumn { ccName = "name", ccType = TextT }
                , CompositeColumn { ccName = "content", ccType = BinaryT }
                , CompositeColumn { ccName = "file_id", ccType = BigIntT }
                ]
  }

tableMailEvents :: Table
tableMailEvents = tblTable
  { tblName        = "mail_events"
  , tblVersion     = 1
  , tblColumns = [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                 , tblColumn { colName     = "mail_id"
                             , colType     = BigIntT
                             , colNullable = False
                             }
                 , tblColumn { colName = "event", colType = TextT, colNullable = False }
                 , tblColumn { colName = "event_read", colType = TimestampWithZoneT }
                 ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [(fkOnColumn "mail_id" "mails" "id") { fkOnDelete = ForeignKeyCascade }]
  , tblIndexes     = [indexOnColumn "mail_id"]
  }
