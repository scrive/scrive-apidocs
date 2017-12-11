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
      -- @todo: the index created by introducing the pk supercedes the existing
      -- one on `signatory_link_id`, but we cannot drop it until we have
      -- `hpqtypes-extras-1.5.0.0`.
      runQuery_ $ sqlAlterTable (tblName tableSignatorySMSPins) [
          sqlAddPK (tblName tableSignatorySMSPins)
                   (fromJust . pkOnColumns $ ["signatory_link_id", "phone_number"])
        ]
  }
