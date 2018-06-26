module Chargeable.Tables where

import DB

tableChargeableItems :: Table
tableChargeableItems = tblTable {
  tblName = "chargeable_items"
, tblVersion = 6
, tblColumns = [
    tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
  , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
  , tblColumn { colName = "company_id", colType = BigIntT, colNullable = False }
  , tblColumn { colName = "type", colType = SmallIntT, colNullable = False }
  , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
  , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
  , tblColumn { colName = "quantity", colType = IntegerT, colNullable = False }
  , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
  ]
, tblPrimaryKey = pkOnColumn "id"
, tblForeignKeys = [
    fkOnColumn "company_id" "companies" "id"
  , fkOnColumn "user_group_id" "user_groups" "id"
  , fkOnColumn "user_id" "users" "id"
  , fkOnColumn "document_id" "documents" "id"
  ]
, tblIndexes = [
    indexOnColumn "company_id"
  , indexOnColumn "user_group_id"
  , indexOnColumn "user_id"
  , indexOnColumn "document_id"
  , indexOnColumn "\"time\""
  , indexOnColumn "type"
  , indexOnColumns ["type", "company_id", "\"time\""]
  , indexOnColumns ["type", "user_group_id", "\"time\""]
  ]
}
