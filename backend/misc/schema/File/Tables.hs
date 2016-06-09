module File.Tables where

import DB
import KontraPrelude

tableFiles :: Table
tableFiles = tblTable {
    tblName = "files"
  , tblVersion = 9
  , tblColumns = [
      tblColumn { colName = "id",            colType = BigSerialT,  colNullable = False }
    , tblColumn { colName = "name",          colType = TextT,       colNullable = False }
    , tblColumn { colName = "content",       colType = BinaryT }
    , tblColumn { colName = "amazon_url",    colType = TextT }
    , tblColumn { colName = "size",          colType = IntegerT,    colNullable = False }
    , tblColumn { colName = "checksum",      colType = BinaryT,     colNullable = False }
    , tblColumn { colName = "aes_key",       colType = BinaryT }
    , tblColumn { colName = "aes_iv",        colType = BinaryT }
    , tblColumn { colName = "purged_time",   colType = TimestampWithZoneT }
    , tblColumn { colName = "purge_at",      colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes = [
      -- For fetching files to be purged.
      (indexOnColumn "purge_at") {
        idxWhere = Just "purge_at IS NOT NULL AND purged_time IS NULL"
      }
      -- For fetching files to be uploaded to Amazon.
    , (indexOnColumn "id") {
        idxWhere = Just "content IS NOT NULL"
      }
    ]
  }
