module Doc.SMSPin.Tables where

import DB
import KontraPrelude

tableSignatorySMSPins :: Table
tableSignatorySMSPins = tblTable {
    tblName = "signatory_sms_pins"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "phone_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "pin", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumns ["signatory_link_id", "phone_number"]
  , tblForeignKeys = [
      (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "signatory_link_id"
    , indexOnColumn "phone_number"
    ]
  }
