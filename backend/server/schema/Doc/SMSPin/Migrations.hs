module Doc.SMSPin.Migrations
    (
      addPKToSignatorySMSPin
    ) where

import DB
import Doc.SMSPin.Tables
import KontraPrelude

addPKToSignatorySMSPin :: MonadDB m => Migration m
addPKToSignatorySMSPin = Migration {
    mgrTableName = tblName tableSignatorySMSPins
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableSignatorySMSPins) [
          sqlAddPK (tblName tableSignatorySMSPins)
                   (fromJust . pkOnColumns $ ["phone_number", "signatory_link_id"])
        ]
  }
