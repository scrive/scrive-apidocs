module Doc.SMSPin.Tables where

import DB

tableSignatorySMSPins :: Table
tableSignatorySMSPins = tblTable {
    tblName = "signatory_sms_pins"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "phone_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "pin", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumns ["phone_number", "signatory_link_id"]
  , tblForeignKeys = [
      (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "signatory_link_id"
    , indexOnColumn "phone_number"
    ]
  }
