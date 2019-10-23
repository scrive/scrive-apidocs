module EvidenceLog.Tables (tableEvidenceLog) where

import DB

{- |
Description of the table to store the evidence log.

The main principle of this table is that it should be as independent
as possible from other tables to avoid migration conflicts. Hence the
lack of foreign keys.

Also, it is insert only. No updates, no deletes.
-}
tableEvidenceLog :: Table
tableEvidenceLog = tblTable
  { tblName       = "evidence_log"
  , tblVersion    = 7
  , tblColumns    =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT }
    , tblColumn { colName = "user_id", colType = BigIntT }
    , tblColumn { colName = "email", colType = TextT }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "request_ip_v4", colType = IntegerT }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT }
    , tblColumn { colName = "text", colType = TextT, colNullable = False }
    , tblColumn { colName = "event_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "version_id", colType = TextT, colNullable = False }
    , tblColumn { colName = "api_user", colType = TextT }
    , tblColumn { colName = "affected_signatory_link_id", colType = BigIntT }
    , tblColumn { colName = "message_text", colType = TextT }
    , tblColumn { colName = "client_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "client_name", colType = TextT }
    , tblColumn { colName = "actor", colType = TextT, colNullable = False }
    , tblColumn { colName = "additional_message_text", colType = TextT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes    = [ indexOnColumn "user_id"
                    , indexOnColumn "document_id"
                    , indexOnColumn "email"
                    , indexOnColumn "signatory_link_id"
                    , indexOnColumn "affected_signatory_link_id"
                    ]
  }
