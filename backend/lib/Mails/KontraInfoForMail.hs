module Mails.KontraInfoForMail (
    KontraInfoForMail(..)
  , AddKontraInfoForMail(..)
  , GetKontraInfosForMails(..)
) where

import Control.Monad.Catch
import Data.Aeson
import Data.Int

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import Log.Identifier
import Mails.Model

data KontraInfoForMailType
  = DocumentInvitationMailT
  | DocumentConfirmationMailT
  | OtherDocumentMailT
  deriving (Eq, Ord, Show)

data KontraInfoForMail
  = DocumentInvitationMail DocumentID SignatoryLinkID
  | DocumentConfirmationMail DocumentID SignatoryLinkID
  | OtherDocumentMail DocumentID
  deriving (Eq, Ord, Show)

instance Loggable KontraInfoForMail where
  logValue (DocumentInvitationMail did slid) = object $ [
           "type" .= ("invitation"::Text)
          , identifier did
          , identifier slid
          ]
  logValue (DocumentConfirmationMail did slid) = object
    [ "type" .= ("confirmation" :: Text)
    , identifier did
    , identifier slid
    ]
  logValue (OtherDocumentMail did) = object $ [
            "type" .= ("other"::Text)
          , identifier did
          ]

  logDefaultLabel _ = "mail_info"


instance PQFormat KontraInfoForMailType where
  pqFormat = pqFormat @Int16

instance FromSQL KontraInfoForMailType where
  type PQBase KontraInfoForMailType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return DocumentInvitationMailT
      2 -> return OtherDocumentMailT
      3 -> return DocumentConfirmationMailT
      _ -> throwM RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL KontraInfoForMailType where
  type PQDest KontraInfoForMailType = PQDest Int16
  toSQL DocumentInvitationMailT     = toSQL (1::Int16)
  toSQL OtherDocumentMailT          = toSQL (2::Int16)
  toSQL DocumentConfirmationMailT   = toSQL (3::Int16)

data AddKontraInfoForMail = AddKontraInfoForMail MailID KontraInfoForMail
instance (MonadDB m, MonadThrow m) => DBUpdate m AddKontraInfoForMail Bool where
  update (AddKontraInfoForMail mailid mfdi) = do
    runQuery01 . sqlInsert "kontra_info_for_mails" $ do
      sqlSet "mail_id" mailid
      case mfdi of
        (DocumentInvitationMail did slid) -> do
           sqlSet "mail_type" DocumentInvitationMailT
           sqlSet "document_id" did
           sqlSet "signatory_link_id" slid
        (DocumentConfirmationMail did slid) -> do
           sqlSet "mail_type" DocumentConfirmationMailT
           sqlSet "document_id" did
           sqlSet "signatory_link_id" slid
        (OtherDocumentMail did) -> do
           sqlSet "mail_type" OtherDocumentMailT
           sqlSet "document_id" did

data GetKontraInfosForMails = GetKontraInfosForMails [MailID]
instance (MonadDB m, MonadThrow m) => DBQuery m GetKontraInfosForMails [(MailID, KontraInfoForMail)] where
  query (GetKontraInfosForMails mailids) = do
    runQuery_ . sqlSelect "kontra_info_for_mails" $ do
      sqlWhereIn "mail_id" mailids
      sqlResult "mail_id"
      sqlResult "mail_type"
      sqlResult "document_id"
      sqlResult "signatory_link_id"
      sqlOrderBy "document_id"
    fetchMany fetchKontraInfoForMail

fetchKontraInfoForMail :: (MailID, KontraInfoForMailType, Maybe DocumentID, Maybe SignatoryLinkID) -> (MailID, KontraInfoForMail)
fetchKontraInfoForMail (mid, DocumentInvitationMailT, Just did, Just sig)  = (mid, DocumentInvitationMail did sig)
fetchKontraInfoForMail (_, DocumentInvitationMailT, _, _)  = (unexpectedError "Failed to fetch KontraInfoForMail (DocumentInvitationMailT)")
fetchKontraInfoForMail (mid, DocumentConfirmationMailT, Just did, Just sig)  = (mid, DocumentConfirmationMail did sig)
fetchKontraInfoForMail (_, DocumentConfirmationMailT, _, _)  = (unexpectedError "Failed to fetch KontraInfoForMail (DocumentConfirmationMailT)")
fetchKontraInfoForMail (mid, OtherDocumentMailT, Just did, Nothing)  = (mid, OtherDocumentMail did)
fetchKontraInfoForMail (_, OtherDocumentMailT, _, _)  = (unexpectedError "Failed to fetch KontraInfoForMail (OtherDocumentMailT)")
