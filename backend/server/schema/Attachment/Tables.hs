module Attachment.Tables where

import DB

tableAttachments :: Table
tableAttachments = tblTable
  { tblName        = "attachments"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "title", colType = TextT, colNullable = False }
    , tblColumn { colName = "ctime", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "mtime", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "file_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "shared", colType = BoolT, colNullable = False }
    , tblColumn { colName = "deleted", colType = BoolT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys = [ (fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade
                                                           }
                     , fkOnColumn "file_id" "files" "id"
                     ]
  , tblIndexes     = [indexOnColumn "user_id", indexOnColumn "file_id"]
  }
