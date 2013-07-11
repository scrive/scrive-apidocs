module File.Tables where

import DB

tableFiles :: Table
tableFiles = tblTable {
    tblName = "files"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "content", colType = BinaryT }
    , tblColumn { colName = "amazon_bucket", colType = TextT }
    , tblColumn { colName = "amazon_url", colType = TextT }
    , tblColumn { colName = "size", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "checksum", colType = BinaryT, colNullable = False }
    , tblColumn { colName = "aes_key", colType = BinaryT, colNullable = False }
    , tblColumn { colName = "aes_iv", colType = BinaryT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  , tblPutProperties = do
      -- create the sequence
      kRunRaw "CREATE SEQUENCE files_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      kRunRaw "SELECT setval('files_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM files))"
      -- and finally attach serial default value to files.id
      kRunRaw "ALTER TABLE files ALTER id SET DEFAULT nextval('files_id_seq')"
  }
