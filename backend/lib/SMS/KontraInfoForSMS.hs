module SMS.KontraInfoForSMS (
    KontraInfoForSMS(..)
  , AddKontraInfoForSMS(..)
  , GetKontraInfoForSMS(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Log

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import Log.Identifier
import SMS.Data

data KontraInfoForSMSType = DocumentInvitationSMST | DocumentPinSendoutSMST | OtherDocumentSMST
  deriving (Eq, Ord, Show)

data KontraInfoForSMS =
  DocumentInvitationSMS DocumentID SignatoryLinkID |
  DocumentPinSendoutSMS DocumentID SignatoryLinkID |
  OtherDocumentSMS DocumentID
  deriving (Eq, Ord, Show)


instance PQFormat KontraInfoForSMSType where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL KontraInfoForSMSType where
  type PQBase KontraInfoForSMSType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return DocumentInvitationSMST
      2 -> return DocumentPinSendoutSMST
      3 -> return OtherDocumentSMST
      _ -> throwM RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL KontraInfoForSMSType where
  type PQDest KontraInfoForSMSType = PQDest Int16
  toSQL DocumentInvitationSMST      = toSQL (1::Int16)
  toSQL DocumentPinSendoutSMST      = toSQL (2::Int16)
  toSQL OtherDocumentSMST           = toSQL (3::Int16)

instance Loggable KontraInfoForSMS where
  logValue (DocumentInvitationSMS did slid) = object [
       "type" .= ("invitation" :: String)
      , identifier_ did
      , identifier_ slid
    ]
  logValue (DocumentPinSendoutSMS did slid) = object [
       "type" .= ("pin_sendout" :: String)
      , identifier_ did
      , identifier_ slid
    ]
  logValue (OtherDocumentSMS did) = object [
       "type" .= ("other" :: String)
      , identifier_ did
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
        (OtherDocumentSMS did) -> do
           sqlSet "sms_type" OtherDocumentSMST
           sqlSet "document_id" did

data GetKontraInfoForSMS = GetKontraInfoForSMS ShortMessageID
instance (MonadDB m, MonadThrow m) => DBQuery m GetKontraInfoForSMS (Maybe KontraInfoForSMS) where
  query (GetKontraInfoForSMS smsid) = do
    runQuery_ . sqlSelect "kontra_info_for_smses" $ do
                sqlWhereEq "sms_id" smsid
                sqlResult "sms_type"
                sqlResult "document_id"
                sqlResult "signatory_link_id"
    fetchMaybe fetchKontraInfoForSMS

fetchKontraInfoForSMS :: (KontraInfoForSMSType, Maybe DocumentID, Maybe SignatoryLinkID) -> KontraInfoForSMS
fetchKontraInfoForSMS (DocumentInvitationSMST, Just did, Just sig)  = DocumentInvitationSMS did sig
fetchKontraInfoForSMS (DocumentInvitationSMST, _, _)  = $unexpectedError "Failed to fetch KontraInfoForSMS (DocumentInvitationSMST)"
fetchKontraInfoForSMS (DocumentPinSendoutSMST, Just did, Just sig)  = DocumentPinSendoutSMS did sig
fetchKontraInfoForSMS (DocumentPinSendoutSMST, _, _)  = $unexpectedError "Failed to fetch KontraInfoForSMS (DocumentPinSendoutSMST)"
fetchKontraInfoForSMS (OtherDocumentSMST, Just did, Nothing)  = OtherDocumentSMS did
fetchKontraInfoForSMS (OtherDocumentSMST, _, _)  =  $unexpectedError "Failed to fetch KontraInfoForSMS (OtherDocumentSMST)"
