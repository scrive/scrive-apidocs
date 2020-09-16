module Doc.SMSPin.Migrations
    ( addPKToSignatorySMSPin
    , addSMSPinTypeToSMSMSPin
    , addGeneratedAtToSMSPin
    , addAttemptsToSMSPin
    ) where

import DB

addPKToSignatorySMSPin :: MonadDB m => Migration m
addPKToSignatorySMSPin = Migration
  { mgrTableName = tableName
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        tableName
        [ sqlAddPK tableName
                   (fromJust . pkOnColumns $ ["phone_number", "signatory_link_id"])
        ]
  }
  where tableName = "signatory_sms_pins"

addSMSPinTypeToSMSMSPin :: MonadDB m => Migration m
addSMSPinTypeToSMSMSPin = Migration
  { mgrTableName = tableName
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       tableName
                       [ sqlAddColumn $ tblColumn { colName     = "pin_type"
                                                  , colType     = SmallIntT
                                                  , colNullable = False
                                                  , colDefault  = Just "1"
                                                  }
                       ]
                     runQuery_
                       $ sqlAlterTable tableName [sqlAlterColumn "pin_type" "DROP DEFAULT"]
                     runQuery_ $ sqlAlterTable
                       tableName
                       [ sqlDropPK tableName
                       , sqlAddPK
                         tableName
                         ( fromJust
                         . pkOnColumns
                         $ ["phone_number", "signatory_link_id", "pin_type"]
                         )
                       ]
  }
  where tableName = "signatory_sms_pins"

addGeneratedAtToSMSPin :: MonadDB m => Migration m
addGeneratedAtToSMSPin = Migration
  { mgrTableName = tableName
  , mgrFrom      = 3
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       tableName
                       [ sqlAddColumn $ tblColumn { colName     = "generated_at"
                                                  , colType     = TimestampWithZoneT
                                                  , colNullable = False
                                                  , colDefault  = Just "now()"
                                                  }
                       ]
                     runQuery_ $ sqlAlterTable
                       tableName
                       [sqlAlterColumn "generated_at" "DROP DEFAULT"]
  }
  where tableName = "signatory_sms_pins"

addAttemptsToSMSPin :: MonadDB m => Migration m
addAttemptsToSMSPin = Migration
  { mgrTableName = tableName
  , mgrFrom      = 4
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       tableName
                       [ sqlAddColumn $ tblColumn { colName     = "attempts"
                                                  , colType     = IntegerT
                                                  , colNullable = False
                                                  , colDefault  = Just "0"
                                                  }
                       ]
  }
  where tableName = "signatory_sms_pins"
