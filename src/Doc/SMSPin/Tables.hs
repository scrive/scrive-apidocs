module Doc.SMSPin.Tables where


import DB
import DB.Checks
import KontraPrelude

createSignatorySMSPinsTable :: MonadDB m => Migration m
createSignatorySMSPinsTable =
  Migration {
      mgrTable = tableSignatorySMSPins
    , mgrFrom = 0
    , mgrDo = do
        createTable $ tblTable {
            tblName = "signatory_sms_pins"
          , tblVersion = 1
          , tblColumns = [
              tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
            , tblColumn { colName = "phone_number", colType = TextT, colNullable = False }
            , tblColumn { colName = "pin", colType = TextT, colNullable = False }
            ]
          , tblForeignKeys = [
              (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
            ]
          , tblIndexes = [
              indexOnColumn "signatory_link_id"
            , indexOnColumn "phone_number"
            ]
          }

}



tableSignatorySMSPins :: Table
tableSignatorySMSPins = tblTable {
    tblName = "signatory_sms_pins"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "phone_number", colType = TextT, colNullable = False }
    , tblColumn { colName = "pin", colType = TextT, colNullable = False }
    ]
  , tblForeignKeys = [
      (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "signatory_link_id"
    , indexOnColumn "phone_number"
    ]
  }
