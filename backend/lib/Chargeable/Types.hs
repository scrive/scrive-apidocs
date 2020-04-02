module Chargeable.Types (
    ChargeableItem(..)
  , ChargeableItemEvent(..)
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Int
import Data.Time
import Data.Typeable

import DB
import Doc.DocumentID
import User.UserID
import UserGroup.Types

data ChargeableItem = CIStartingDocument
                    | CIClosingDocument
                    | CISMS
                    | CISMSTelia
                    | CISEBankIDSignature
                    | CISEBankIDAuthentication
                    | CINOBankIDAuthentication
                    | CIDKNemIDAuthentication
                    | CIClosingSignature
                    | CINOBankIDSignature
                    | CIDKNemIDSignature
                    | CIFITupasAuthentication
                    | CIFITupasSignature
                    | CIShareableLink
                    | CIVerimiAuthentication
                    | CIIDINAuthentication
                    | CIIDINSignature
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat ChargeableItem where
  pqFormat = pqFormat @Int16

instance FromSQL ChargeableItem where
  type PQBase ChargeableItem = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      -- Note:
      -- If changing this, please also update `pure_sql/invoice_stats.sql`
      1  -> return CISMS
      2  -> return CISEBankIDSignature
      3  -> return CISEBankIDAuthentication
      4  -> return CINOBankIDAuthentication
      5  -> return CISMSTelia
      6  -> return CIStartingDocument
      7  -> return CIDKNemIDAuthentication
      8  -> return CIClosingDocument
      9  -> return CIClosingSignature
      10 -> return CINOBankIDSignature
      11 -> return CIDKNemIDSignature
      12 -> return CIFITupasAuthentication
      13 -> return CIShareableLink
      14 -> return CIVerimiAuthentication
      15 -> return CIIDINAuthentication
      16 -> return CIIDINSignature
      17 -> return CIFITupasSignature
      _  -> throwM RangeError { reRange = [(1, 17)], reValue = n }

instance ToSQL ChargeableItem where
  type PQDest ChargeableItem = PQDest Int16
  toSQL CISMS                    = toSQL (1 :: Int16)
  toSQL CISEBankIDSignature      = toSQL (2 :: Int16)
  toSQL CISEBankIDAuthentication = toSQL (3 :: Int16)
  toSQL CINOBankIDAuthentication = toSQL (4 :: Int16)
  toSQL CISMSTelia               = toSQL (5 :: Int16)
  toSQL CIStartingDocument       = toSQL (6 :: Int16)
  toSQL CIDKNemIDAuthentication  = toSQL (7 :: Int16)
  toSQL CIClosingDocument        = toSQL (8 :: Int16)
  toSQL CIClosingSignature       = toSQL (9 :: Int16)
  toSQL CINOBankIDSignature      = toSQL (10 :: Int16)
  toSQL CIDKNemIDSignature       = toSQL (11 :: Int16)
  toSQL CIFITupasAuthentication  = toSQL (12 :: Int16)
  toSQL CIShareableLink          = toSQL (13 :: Int16)
  toSQL CIVerimiAuthentication   = toSQL (14 :: Int16)
  toSQL CIIDINAuthentication     = toSQL (15 :: Int16)
  toSQL CIIDINSignature          = toSQL (16 :: Int16)
  toSQL CIFITupasSignature       = toSQL (17 :: Int16)

-- | This type should be used only for serialization. Punishment will be imposed
-- on anybody using it in any other way.
data ChargeableItemEvent = ChargeableItemEvent
  { chargeableItemEventTime :: UTCTime
  , chargeableItemEventType :: ChargeableItem
  , chargeableItemEventDocumentId :: DocumentID
  , chargeableItemEventUserId :: UserID
  , chargeableItemEventUserGroupId :: UserGroupID
  , chargeableItemEventQuantity :: Int32
  } deriving (Show, Eq)

instance ToJSON ChargeableItemEvent where
  toJSON ChargeableItemEvent {..} = object
    [ "type" .= ("chargeable_item" :: Text)
    , "time" .= chargeableItemEventTime
    , ("chargeable_item_type", toJSON $ eventTypeToJSON chargeableItemEventType)
    , "user_id" .= chargeableItemEventUserId
    , "document_id" .= chargeableItemEventDocumentId
    , "quantity" .= chargeableItemEventQuantity
    , "user_group_id" .= chargeableItemEventUserGroupId
    ]
    where
      eventTypeToJSON :: ChargeableItem -> Text
      eventTypeToJSON CISMS                    = "sms"
      eventTypeToJSON CISEBankIDSignature      = "se_bank_id_signature"
      eventTypeToJSON CISEBankIDAuthentication = "se_bank_id_authentication"
      eventTypeToJSON CINOBankIDAuthentication = "no_bank_id_authentication"
      eventTypeToJSON CISMSTelia               = "sms_telia"
      eventTypeToJSON CIStartingDocument       = "starting_document"
      eventTypeToJSON CIDKNemIDAuthentication  = "dk_nem_id_authentication"
      eventTypeToJSON CIClosingDocument        = "closing_document"
      eventTypeToJSON CIClosingSignature       = "closing_signature"
      eventTypeToJSON CINOBankIDSignature      = "no_bank_id_signature"
      eventTypeToJSON CIDKNemIDSignature       = "dk_nem_id_signature"
      eventTypeToJSON CIFITupasAuthentication  = "fi_tupas_authentication"
      eventTypeToJSON CIFITupasSignature       = "fi_tupas_signature"
      eventTypeToJSON CIShareableLink          = "shareable_link"
      eventTypeToJSON CIVerimiAuthentication   = "verimi_authentication"
      eventTypeToJSON CIIDINAuthentication     = "idin_authentication"
      eventTypeToJSON CIIDINSignature          = "idin_signature"
