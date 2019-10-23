module SMS.KontraInfoForSMS (
    KontraInfoForSMS(..)
  , AddKontraInfoForSMS(..)
  , getUnreadSMSEvents
  ) where

import Control.Monad.Catch
import Data.Int
import Log

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import Log.Identifier
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
  deriving (Eq, Ord, Show)


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

data AddKontraInfoForSMS = AddKontraInfoForSMS ShortMessageID KontraInfoForSMS
instance (MonadDB m, MonadThrow m) => DBUpdate m AddKontraInfoForSMS Bool where
  update (AddKontraInfoForSMS smsid kifs) = do
    runQuery01 . sqlInsert "kontra_info_for_smses" $ do
      sqlSet "sms_id" smsid
      case kifs of
        (DocumentInvitationSMS did slid) -> do
          sqlSet "sms_type"          DocumentInvitationSMST
          sqlSet "document_id"       did
          sqlSet "signatory_link_id" slid
        (DocumentPinSendoutSMS did slid) -> do
          sqlSet "sms_type"          DocumentPinSendoutSMST
          sqlSet "document_id"       did
          sqlSet "signatory_link_id" slid
        (DocumentPartyNotificationSMS did slid) -> do
          sqlSet "sms_type"          DocumentPartyNotificationSMST
          sqlSet "document_id"       did
          sqlSet "signatory_link_id" slid
        (OtherDocumentSMS did) -> do
          sqlSet "sms_type"    OtherDocumentSMST
          sqlSet "document_id" did

fetchKontraInfoForSMS
  :: (Maybe KontraInfoForSMSType, Maybe DocumentID, Maybe SignatoryLinkID)
  -> Maybe KontraInfoForSMS
fetchKontraInfoForSMS (Nothing, _, _) = Nothing
fetchKontraInfoForSMS (Just DocumentInvitationSMST, Just did, Just sig) =
  Just $ DocumentInvitationSMS did sig
fetchKontraInfoForSMS (Just DocumentInvitationSMST, _, _) =
  unexpectedError "Failed to fetch KontraInfoForSMS (DocumentInvitationSMST)"
fetchKontraInfoForSMS (Just DocumentPinSendoutSMST, Just did, Just sig) =
  Just $ DocumentPinSendoutSMS did sig
fetchKontraInfoForSMS (Just DocumentPinSendoutSMST, _, _) =
  unexpectedError "Failed to fetch KontraInfoForSMS (DocumentPinSendoutSMST)"
fetchKontraInfoForSMS (Just DocumentPartyNotificationSMST, Just did, Just sig) =
  Just $ DocumentPartyNotificationSMS did sig
fetchKontraInfoForSMS (Just DocumentPartyNotificationSMST, _, _) =
  unexpectedError "Failed to fetch KontraInfoForSMS (DocumentPartyNotificationSMST)"
fetchKontraInfoForSMS (Just OtherDocumentSMST, Just did, Nothing) =
  Just $ OtherDocumentSMS did
fetchKontraInfoForSMS (Just OtherDocumentSMST, _, _) =
  unexpectedError "Failed to fetch KontraInfoForSMS (OtherDocumentSMST)"

fetchEvent
  :: ( SMSEventID
     , ShortMessageID
     , SMSEvent
     , Text
     , Maybe KontraInfoForSMSType
     , Maybe DocumentID
     , Maybe SignatoryLinkID
     )
  -> (SMSEventID, ShortMessageID, SMSEvent, Text, Maybe KontraInfoForSMS)
fetchEvent (eid, smsid, e, msisdn, mkifsmst, mkifsmsdid, mkifsmsslid) =
  (eid, smsid, e, msisdn, mkifsms)
  where mkifsms = fetchKontraInfoForSMS (mkifsmst, mkifsmsdid, mkifsmsslid)

getUnreadSMSEvents
  :: MonadDB m => m [(SMSEventID, ShortMessageID, SMSEvent, Text, Maybe KontraInfoForSMS)]
getUnreadSMSEvents = do
  runQuery_ . sqlSelect "sms_events" $ do
    sqlJoinOn "smses" "smses.id = sms_events.sms_id"
    sqlLeftJoinOn "kontra_info_for_smses kifsms" "sms_events.sms_id = kifsms.sms_id"
    sqlResult "sms_events.id"
    sqlResult "sms_events.sms_id"
    sqlResult "sms_events.event"
    sqlResult "smses.msisdn"
    sqlResult "kifsms.sms_type"
    sqlResult "kifsms.document_id"
    sqlResult "kifsms.signatory_link_id"
    sqlWhere "sms_events.event_read IS NULL"
    sqlOrderBy "kifsms.document_id"
    sqlOrderBy "smses.id"
    sqlOrderBy "sms_events.id"
  fetchMany fetchEvent
