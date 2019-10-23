module Doc.SMSPin.Tables where

import DB

tableSignatorySMSPins :: Table
tableSignatorySMSPins = tblTable
  { tblName        = "signatory_sms_pins"
  , tblVersion     = 4
  , tblColumns     =
    [ tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "phone_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "pin", colType = TextT, colNullable = False }
    , tblColumn { colName = "pin_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName     = "generated_at"
                , colType     = TimestampWithZoneT
                , colNullable = False
                }
    ]
  , tblPrimaryKey  = pkOnColumns ["phone_number", "signatory_link_id", "pin_type"]
  , tblForeignKeys =
    [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [indexOnColumn "signatory_link_id", indexOnColumn "phone_number"]
  }
