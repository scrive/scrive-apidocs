module SMS.KontraInfoForSMS
  ( KontraInfoForSMS(..)
  , AddKontraInfoForSMS(..)
  , getUnreadSMSEvents
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Int
import GHC.Generics
import qualified Data.Text as T

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import Log.Identifier
import SMS.FromKontra.Tables
import SMS.Types

data KontraInfoForSMSType =
  DocumentInvitationSMST |
  DocumentPinSendoutSMST |
  DocumentPartyNotificationSMST |
  OtherDocumentSMST
  deriving (Eq, Ord, Show)

data KontraInfoForSMS =
  DocumentInvitationSMS DocumentID SignatoryLinkID |
  DocumentPinSendoutSMS DocumentID SignatoryLinkID |
  DocumentPartyNotificationSMS DocumentID SignatoryLinkID |
  OtherDocumentSMS DocumentID
  deriving (Eq, Ord, Show, Generic)

-- For logging
instance ToJSON KontraInfoForSMS

instance PQFormat KontraInfoForSMSType where
  pqFormat = pqFormat @Int16

instance FromSQL KontraInfoForSMSType where
  type PQBase KontraInfoForSMSType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return DocumentInvitationSMST
      2 -> return DocumentPinSendoutSMST
      3 -> return OtherDocumentSMST
      4 -> return DocumentPartyNotificationSMST
      _ -> throwM RangeError { reRange = [(1, 3)], reValue = n }

instance ToSQL KontraInfoForSMSType where
  type PQDest KontraInfoForSMSType = PQDest Int16
  toSQL DocumentInvitationSMST        = toSQL (1 :: Int16)
  toSQL DocumentPinSendoutSMST        = toSQL (2 :: Int16)
  toSQL OtherDocumentSMST             = toSQL (3 :: Int16)
  toSQL DocumentPartyNotificationSMST = toSQL (4 :: Int16)

instance Loggable KontraInfoForSMS where
  logValue (DocumentInvitationSMS did slid) =
    object ["type" .= ("invitation" :: String), identifier did, identifier slid]
  logValue (DocumentPinSendoutSMS did slid) =
    object ["type" .= ("pin_sendout" :: String), identifier did, identifier slid]
  logValue (DocumentPartyNotificationSMS did slid) =
    object ["type" .= ("notification" :: String), identifier did, identifier slid]
  logValue (OtherDocumentSMS did) =
    object ["type" .= ("other" :: String), identifier did]

  logDefaultLabel _ = "sms_document_info"

data AddKontraInfoForSMS = AddKontraInfoForSMS ShortMessageID [KontraInfoForSMS]
instance (MonadDB m, MonadThrow m) => DBUpdate m AddKontraInfoForSMS Bool where
  dbUpdate (AddKontraInfoForSMS _     []  ) = pure True
  dbUpdate (AddKontraInfoForSMS smsid kifs) = do
    n <- runQuery . sqlInsert "kontra_info_for_smses" $ do
      let values = kontraInfoToTuple <$> kifs
      sqlSet "sms_id" smsid
      sqlSetList "sms_type"          (view _1 <$> values)
      sqlSetList "document_id"       (view _2 <$> values)
      sqlSetList "signatory_link_id" (view _3 <$> values)
    pure $ n == length kifs
    where
      kontraInfoToTuple kifs' = case kifs' of
        (DocumentInvitationSMS did slid) -> (DocumentInvitationSMST, did, Just slid)
        (DocumentPinSendoutSMS did slid) -> (DocumentPinSendoutSMST, did, Just slid)
        (DocumentPartyNotificationSMS did slid) ->
          (DocumentPartyNotificationSMST, did, Just slid)
        (OtherDocumentSMS did) -> (OtherDocumentSMST, did, Nothing)

data KontraInfoForSMSAggregate
  = KontraInfoForSMSAggregate
  { kontraInfoForSMSType :: KontraInfoForSMSType
  , kontraInfoForSMSDocumentId :: DocumentID
  , kontraInfoForSMSSignatoryLinkId :: Maybe SignatoryLinkID
  }

type instance CompositeRow KontraInfoForSMSAggregate
  = (KontraInfoForSMSType, DocumentID, Maybe SignatoryLinkID)

instance PQFormat KontraInfoForSMSAggregate where
  pqFormat = compositeTypePqFormat ctKontraForSMSAggregate

instance CompositeFromSQL KontraInfoForSMSAggregate where
  toComposite (sms_type, did, mslid) = KontraInfoForSMSAggregate
    { kontraInfoForSMSType            = sms_type
    , kontraInfoForSMSDocumentId      = did
    , kontraInfoForSMSSignatoryLinkId = mslid
    }

fetchKontraInfoForSMS :: KontraInfoForSMSAggregate -> KontraInfoForSMS
fetchKontraInfoForSMS KontraInfoForSMSAggregate { kontraInfoForSMSType = sms_type, kontraInfoForSMSDocumentId = did, kontraInfoForSMSSignatoryLinkId = mslid }
  = mapper mslid
  where
    mapper = case sms_type of
      DocumentInvitationSMST -> maybe
        (unexpectedError "Failed to fetch KontraInfoForSMS (DocumentInvitationSMST)")
        (DocumentInvitationSMS did)
      DocumentPinSendoutSMST -> maybe
        (unexpectedError "Failed to fetch KontraInfoForSMS (DocumentPinSendoutSMST)")
        (DocumentPinSendoutSMS did)
      DocumentPartyNotificationSMST -> maybe
        (unexpectedError
          "Failed to fetch KontraInfoForSMS (DocumentPartyNotificationSMST)"
        )
        (DocumentPartyNotificationSMS did)
      OtherDocumentSMST -> maybe
        (OtherDocumentSMS did)
        (const $ unexpectedError "Failed to fetch KontraInfoForSMS (OtherDocumentSMST)")

fetchEvent
  :: ( SMSEventID
     , ShortMessageID
     , SMSEvent
     , Text
     , CompositeArray1 KontraInfoForSMSAggregate
     )
  -> (SMSEventID, ShortMessageID, SMSEvent, Text, [KontraInfoForSMS])
fetchEvent (eid, smsid, evt, msisdn, kifss) = (eid, smsid, evt, msisdn, kifs)
  where kifs = fetchKontraInfoForSMS <$> unCompositeArray1 kifss

getUnreadSMSEvents
  :: MonadDB m => m [(SMSEventID, ShortMessageID, SMSEvent, Text, [KontraInfoForSMS])]
getUnreadSMSEvents = do
  runQuery_ . sqlSelect "smses s" $ do
    sqlResult "e.id"
    sqlResult "e.sms_id"
    sqlResult "e.event"
    sqlResult "s.msisdn"
    sqlResult . mkSQL $ T.concat
      [ "ARRAY_AGG((kifs.sms_type, kifs.document_id, kifs.signatory_link_id)::"
      , unRawSQL $ ctName ctKontraForSMSAggregate
      , ")"
      ]
    sqlJoinOn "sms_events e" "s.id = e.sms_id"
    sqlLeftJoinOn "kontra_info_for_smses kifs" "e.sms_id = kifs.sms_id"
    sqlWhere "e.event_read IS NULL"
    sqlOrderBy "s.id"
    sqlOrderBy "e.id"
    sqlGroupBy "s.id"
    sqlGroupBy "e.id"
  fetchMany fetchEvent
