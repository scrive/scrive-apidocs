module SMS.KontraInfoForSMS (
    KontraInfoForSMS(..)
  , AddKontraInfoForSMS(..)
  , GetKontraInfosForSMSes(..)
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
      _ -> throwM RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL KontraInfoForSMSType where
  type PQDest KontraInfoForSMSType = PQDest Int16
  toSQL DocumentInvitationSMST        = toSQL (1::Int16)
  toSQL DocumentPinSendoutSMST        = toSQL (2::Int16)
  toSQL OtherDocumentSMST             = toSQL (3::Int16)
  toSQL DocumentPartyNotificationSMST = toSQL (4::Int16)

instance Loggable KontraInfoForSMS where
  logValue (DocumentInvitationSMS did slid) = object [
       "type" .= ("invitation" :: String)
      , identifier did
      , identifier slid
    ]
  logValue (DocumentPinSendoutSMS did slid) = object [
       "type" .= ("pin_sendout" :: String)
      , identifier did
      , identifier slid
    ]
  logValue (DocumentPartyNotificationSMS did slid) = object [
       "type" .= ("notification" :: String)
      , identifier did
      , identifier slid
    ]
  logValue (OtherDocumentSMS did) = object [
       "type" .= ("other" :: String)
      , identifier did
    ]

  logDefaultLabel _ = "sms_document_info"

data AddKontraInfoForSMS = AddKontraInfoForSMS ShortMessageID KontraInfoForSMS
instance (MonadDB m, MonadThrow m) => DBUpdate m AddKontraInfoForSMS Bool where
  update (AddKontraInfoForSMS smsid kifs) = do
    runQuery01 . sqlInsert "kontra_info_for_smses" $ do
      sqlSet "sms_id" smsid
      case kifs of
        (DocumentInvitationSMS did slid) -> do
           sqlSet "sms_type" DocumentInvitationSMST
           sqlSet "document_id" did
           sqlSet "signatory_link_id" slid
        (DocumentPinSendoutSMS did slid) -> do
           sqlSet "sms_type" DocumentPinSendoutSMST
           sqlSet "document_id" did
           sqlSet "signatory_link_id" slid
        (DocumentPartyNotificationSMS did slid) -> do
           sqlSet "sms_type" DocumentPartyNotificationSMST
           sqlSet "document_id" did
           sqlSet "signatory_link_id" slid
        (OtherDocumentSMS did) -> do
           sqlSet "sms_type" OtherDocumentSMST
           sqlSet "document_id" did

data GetKontraInfosForSMSes = GetKontraInfosForSMSes [ShortMessageID]
instance (MonadDB m, MonadThrow m) => DBQuery m GetKontraInfosForSMSes [(ShortMessageID, KontraInfoForSMS)] where
  query (GetKontraInfosForSMSes smsids) = do
    runQuery_ . sqlSelect "kontra_info_for_smses" $ do
                sqlWhereIn "sms_id" smsids
                sqlResult "sms_id"
                sqlResult "sms_type"
                sqlResult "document_id"
                sqlResult "signatory_link_id"
                sqlOrderBy "document_id"
    fetchMany fetchKontraInfoForSMS

fetchKontraInfoForSMS :: (ShortMessageID, KontraInfoForSMSType, Maybe DocumentID, Maybe SignatoryLinkID) -> (ShortMessageID, KontraInfoForSMS)
fetchKontraInfoForSMS (smsid, DocumentInvitationSMST, Just did, Just sig)  =
  (smsid, DocumentInvitationSMS did sig)
fetchKontraInfoForSMS (_, DocumentInvitationSMST, _, _)  =
  unexpectedError "Failed to fetch KontraInfoForSMS (DocumentInvitationSMST)"
fetchKontraInfoForSMS (smsid, DocumentPinSendoutSMST, Just did, Just sig)  =
  (smsid, DocumentPinSendoutSMS did sig)
fetchKontraInfoForSMS (_, DocumentPinSendoutSMST, _, _)  =
  unexpectedError "Failed to fetch KontraInfoForSMS (DocumentPinSendoutSMST)"
fetchKontraInfoForSMS (smsid, DocumentPartyNotificationSMST, Just did, Just sig) =
  (smsid, DocumentPartyNotificationSMS did sig)
fetchKontraInfoForSMS (_, DocumentPartyNotificationSMST, _, _) =
  unexpectedError "Failed to fetch KontraInfoForSMS (DocumentPartyNotificationSMST)"
fetchKontraInfoForSMS (smsid, OtherDocumentSMST, Just did, Nothing)  =
  (smsid, OtherDocumentSMS did)
fetchKontraInfoForSMS (_, OtherDocumentSMST, _, _)  =
  unexpectedError "Failed to fetch KontraInfoForSMS (OtherDocumentSMST)"
