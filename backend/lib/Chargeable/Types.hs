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
                    | CISEBankIDSignatureFinished
                    | CISEBankIDAuthenticationFinished
                    | CINOBankIDAuthenticationFinished
                    | CIDKNemIDAuthenticationFinished
                    | CIClosingSignature
                    | CINOBankIDSignatureFinished
                    | CIDKNemIDSignatureFinished
                    | CIFITupasAuthenticationFinished
                    | CIFITupasSignatureFinished
                    | CIShareableLink
                    | CIVerimiAuthenticationFinished
                    | CIIDINAuthenticationFinished
                    | CIIDINSignatureFinished
                    | CIOnfidoDocumentCheckSignatureFinished
                    | CIOnfidoDocumentAndPhotoCheckSignatureFinished
                    | CISEBankIDSignatureStarted
                    | CISEBankIDAuthenticationStarted
                    | CINOBankIDAuthenticationStarted
                    | CIDKNemIDAuthenticationStarted
                    | CINOBankIDSignatureStarted
                    | CIDKNemIDSignatureStarted
                    | CIFITupasAuthenticationStarted
                    | CIFITupasSignatureStarted
                    | CIVerimiAuthenticationStarted
                    | CIIDINAuthenticationStarted
                    | CIIDINSignatureStarted
                    | CIOnfidoDocumentCheckSignatureStarted
                    | CIOnfidoDocumentAndPhotoCheckSignatureStarted
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
      2  -> return CISEBankIDSignatureFinished
      3  -> return CISEBankIDAuthenticationFinished
      4  -> return CINOBankIDAuthenticationFinished
      5  -> return CISMSTelia
      6  -> return CIStartingDocument
      7  -> return CIDKNemIDAuthenticationFinished
      8  -> return CIClosingDocument
      9  -> return CIClosingSignature
      10 -> return CINOBankIDSignatureFinished
      11 -> return CIDKNemIDSignatureFinished
      12 -> return CIFITupasAuthenticationFinished
      13 -> return CIShareableLink
      14 -> return CIVerimiAuthenticationFinished
      15 -> return CIIDINAuthenticationFinished
      16 -> return CIIDINSignatureFinished
      17 -> return CIFITupasSignatureFinished
      18 -> return CIOnfidoDocumentCheckSignatureFinished
      19 -> return CIOnfidoDocumentAndPhotoCheckSignatureFinished
      20 -> return CISEBankIDSignatureStarted
      21 -> return CISEBankIDAuthenticationStarted
      22 -> return CINOBankIDAuthenticationStarted
      23 -> return CIDKNemIDAuthenticationStarted
      24 -> return CINOBankIDSignatureStarted
      25 -> return CIDKNemIDSignatureStarted
      26 -> return CIFITupasAuthenticationStarted
      27 -> return CIFITupasSignatureStarted
      28 -> return CIVerimiAuthenticationStarted
      29 -> return CIIDINAuthenticationStarted
      30 -> return CIIDINSignatureStarted
      31 -> return CIOnfidoDocumentCheckSignatureStarted
      32 -> return CIOnfidoDocumentAndPhotoCheckSignatureStarted

      _  -> throwM RangeError { reRange = [(1, 24)], reValue = n }

instance ToSQL ChargeableItem where
  type PQDest ChargeableItem = PQDest Int16
  toSQL CISMS                     = toSQL (1 :: Int16)
  toSQL CISEBankIDSignatureFinished = toSQL (2 :: Int16)
  toSQL CISEBankIDAuthenticationFinished = toSQL (3 :: Int16)
  toSQL CINOBankIDAuthenticationFinished = toSQL (4 :: Int16)
  toSQL CISMSTelia                = toSQL (5 :: Int16)
  toSQL CIStartingDocument        = toSQL (6 :: Int16)
  toSQL CIDKNemIDAuthenticationFinished = toSQL (7 :: Int16)
  toSQL CIClosingDocument         = toSQL (8 :: Int16)
  toSQL CIClosingSignature        = toSQL (9 :: Int16)
  toSQL CINOBankIDSignatureFinished = toSQL (10 :: Int16)
  toSQL CIDKNemIDSignatureFinished = toSQL (11 :: Int16)
  toSQL CIFITupasAuthenticationFinished = toSQL (12 :: Int16)
  toSQL CIShareableLink           = toSQL (13 :: Int16)
  toSQL CIVerimiAuthenticationFinished = toSQL (14 :: Int16)
  toSQL CIIDINAuthenticationFinished = toSQL (15 :: Int16)
  toSQL CIIDINSignatureFinished   = toSQL (16 :: Int16)
  toSQL CIFITupasSignatureFinished = toSQL (17 :: Int16)
  toSQL CIOnfidoDocumentCheckSignatureFinished = toSQL (18 :: Int16)
  toSQL CIOnfidoDocumentAndPhotoCheckSignatureFinished = toSQL (19 :: Int16)
  toSQL CISEBankIDSignatureStarted = toSQL (20 :: Int16)
  toSQL CISEBankIDAuthenticationStarted = toSQL (21 :: Int16)
  toSQL CINOBankIDAuthenticationStarted = toSQL (22 :: Int16)
  toSQL CIDKNemIDAuthenticationStarted = toSQL (23 :: Int16)
  toSQL CINOBankIDSignatureStarted = toSQL (24 :: Int16)
  toSQL CIDKNemIDSignatureStarted = toSQL (25 :: Int16)
  toSQL CIFITupasAuthenticationStarted = toSQL (26 :: Int16)
  toSQL CIFITupasSignatureStarted = toSQL (27 :: Int16)
  toSQL CIVerimiAuthenticationStarted = toSQL (28 :: Int16)
  toSQL CIIDINAuthenticationStarted = toSQL (29 :: Int16)
  toSQL CIIDINSignatureStarted    = toSQL (30 :: Int16)
  toSQL CIOnfidoDocumentCheckSignatureStarted = toSQL (31 :: Int16)
  toSQL CIOnfidoDocumentAndPhotoCheckSignatureStarted = toSQL (32 :: Int16)


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
      eventTypeToJSON CISMS = "sms"
      eventTypeToJSON CISEBankIDSignatureFinished = "se_bank_id_signature_finished"
      eventTypeToJSON CISEBankIDAuthenticationFinished =
        "se_bank_id_authentication_finished"
      eventTypeToJSON CINOBankIDAuthenticationFinished =
        "no_bank_id_authentication_finished"
      eventTypeToJSON CISMSTelia         = "sms_telia"
      eventTypeToJSON CIStartingDocument = "starting_document"
      eventTypeToJSON CIDKNemIDAuthenticationFinished =
        "dk_nem_id_authentication_finished"
      eventTypeToJSON CIClosingDocument           = "closing_document"
      eventTypeToJSON CIClosingSignature          = "closing_signature"
      eventTypeToJSON CINOBankIDSignatureFinished = "no_bank_id_signature_finished"
      eventTypeToJSON CIDKNemIDSignatureFinished  = "dk_nem_id_signature_finished"
      eventTypeToJSON CIFITupasAuthenticationFinished =
        "fi_tupas_authentication_finished"
      eventTypeToJSON CIFITupasSignatureFinished     = "fi_tupas_signature_finished"
      eventTypeToJSON CIShareableLink                = "shareable_link"
      eventTypeToJSON CIVerimiAuthenticationFinished = "verimi_authentication_finished"
      eventTypeToJSON CIIDINAuthenticationFinished   = "idin_authentication_finished"
      eventTypeToJSON CIIDINSignatureFinished        = "idin_signature_finished"
      eventTypeToJSON CIOnfidoDocumentCheckSignatureFinished =
        "onfido_document_signature_finished"
      eventTypeToJSON CIOnfidoDocumentAndPhotoCheckSignatureFinished =
        "onfido_document_and_face_signature_finished"
      eventTypeToJSON CISEBankIDSignatureStarted = "se_bank_id_signature_started"
      eventTypeToJSON CISEBankIDAuthenticationStarted =
        "se_bank_id_authentication_started"
      eventTypeToJSON CINOBankIDAuthenticationStarted =
        "no_bank_id_authentication_started"
      eventTypeToJSON CIDKNemIDAuthenticationStarted = "dk_nem_id_authentication_started"
      eventTypeToJSON CINOBankIDSignatureStarted     = "no_bank_id_signature_started"
      eventTypeToJSON CIDKNemIDSignatureStarted      = "dk_nem_id_signature_started"
      eventTypeToJSON CIFITupasAuthenticationStarted = "fi_tupas_authentication_started"
      eventTypeToJSON CIFITupasSignatureStarted      = "fi_tupas_signature_started"
      eventTypeToJSON CIVerimiAuthenticationStarted  = "verimi_authentication_started"
      eventTypeToJSON CIIDINAuthenticationStarted    = "idin_authentication_started"
      eventTypeToJSON CIIDINSignatureStarted         = "idin_signature_started"
      eventTypeToJSON CIOnfidoDocumentCheckSignatureStarted =
        "onfido_document_signature_started"
      eventTypeToJSON CIOnfidoDocumentAndPhotoCheckSignatureStarted =
        "onfido_document_and_face_signature_started"

