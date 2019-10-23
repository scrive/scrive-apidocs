module Doc.SMSPin.Migrations
    ( addPKToSignatorySMSPin
    , addSMSPinTypeToSMSMSPin
    , addGeneratedAtToSMSPin
    ) where

import DB
import Doc.SMSPin.Tables

addPKToSignatorySMSPin :: MonadDB m => Migration m
addPKToSignatorySMSPin = Migration
  { mgrTableName = tblName tableSignatorySMSPins
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableSignatorySMSPins)
                       [ sqlAddPK
                           (tblName tableSignatorySMSPins)
                           (fromJust . pkOnColumns $ ["phone_number", "signatory_link_id"])
                       ]
  }

addSMSPinTypeToSMSMSPin :: MonadDB m => Migration m
addSMSPinTypeToSMSMSPin = Migration
  { mgrTableName = tblName tableSignatorySMSPins
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableSignatorySMSPins)
                       [ sqlAddColumn $ tblColumn { colName     = "pin_type"
                                                  , colType     = SmallIntT
                                                  , colNullable = False
                                                  , colDefault  = Just "1"
                                                  }
                       ]
                     runQuery_ $ sqlAlterTable (tblName tableSignatorySMSPins)
                                               [sqlAlterColumn "pin_type" "DROP DEFAULT"]
                     runQuery_ $ sqlAlterTable
                       (tblName tableSignatorySMSPins)
                       [ sqlDropPK (tblName tableSignatorySMSPins)
                       , sqlAddPK
                         (tblName tableSignatorySMSPins)
                         ( fromJust
                         . pkOnColumns
                         $ ["phone_number", "signatory_link_id", "pin_type"]
                         )
                       ]
  }

addGeneratedAtToSMSPin :: MonadDB m => Migration m
addGeneratedAtToSMSPin = Migration
  { mgrTableName = tblName tableSignatorySMSPins
  , mgrFrom      = 3
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableSignatorySMSPins)
                       [ sqlAddColumn $ tblColumn { colName     = "generated_at"
                                                  , colType     = TimestampWithZoneT
                                                  , colNullable = False
                                                  , colDefault  = Just "now()"
                                                  }
                       ]
                     runQuery_ $ sqlAlterTable
                       (tblName tableSignatorySMSPins)
                       [sqlAlterColumn "generated_at" "DROP DEFAULT"]
  }
