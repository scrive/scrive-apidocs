module File.Tables where

import DB
import KontraPrelude

tableFiles :: Table
tableFiles = tblTable {
    tblName = "files"
  , tblVersion = 6
  , tblColumns = [
      tblColumn { colName = "id",            colType = BigSerialT,  colNullable = False }
    , tblColumn { colName = "name",          colType = TextT,       colNullable = False }
    , tblColumn { colName = "content",       colType = BinaryT }
    , tblColumn { colName = "amazon_bucket", colType = TextT }
    , tblColumn { colName = "amazon_url",    colType = TextT }
    , tblColumn { colName = "size",          colType = IntegerT,    colNullable = False }
    , tblColumn { colName = "checksum",      colType = BinaryT,     colNullable = False }
    , tblColumn { colName = "aes_key",       colType = BinaryT }
    , tblColumn { colName = "aes_iv",        colType = BinaryT }
    , tblColumn { colName = "purged_time",   colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }
