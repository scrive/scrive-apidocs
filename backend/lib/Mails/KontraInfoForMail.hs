module Mails.KontraInfoForMail (
    KontraInfoForMail(..)
  , AddKontraInfoForMail(..)
  , GetKontraInfoForMail(..)
) where

import Control.Monad.Catch
import Data.Aeson
import Data.Int
import qualified Data.Text as T

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import KontraPrelude
import Log.Identifier
import Mails.Model

data KontraInfoForMailType = DocumentInvitationMailT | OtherDocumentMailT
  deriving (Eq, Ord, Show)

data KontraInfoForMail
  = DocumentInvitationMail DocumentID SignatoryLinkID
  | OtherDocumentMail DocumentID
  deriving (Eq, Ord, Show)

instance Loggable KontraInfoForMail where
  logValue (DocumentInvitationMail did slid) = object $ [
           "type" .= ("invitation"::T.Text)
          , identifier_ did
          , identifier_ slid
          ]
  logValue (OtherDocumentMail did) = object $ [
            "type" .= ("other"::T.Text)
          , identifier_ did
          ]

  logDefaultLabel _ = "mail_info"


instance PQFormat KontraInfoForMailType where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL KontraInfoForMailType where
  type PQBase KontraInfoForMailType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return DocumentInvitationMailT
      2 -> return OtherDocumentMailT
      _ -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL KontraInfoForMailType where
  type PQDest KontraInfoForMailType = PQDest Int16
  toSQL DocumentInvitationMailT     = toSQL (1::Int16)
  toSQL OtherDocumentMailT          = toSQL (2::Int16)

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
        (OtherDocumentMail did) -> do
           sqlSet "mail_type" OtherDocumentMailT
           sqlSet "document_id" did

data GetKontraInfoForMail = GetKontraInfoForMail MailID
instance (MonadDB m, MonadThrow m) => DBQuery m GetKontraInfoForMail (Maybe KontraInfoForMail) where
  query (GetKontraInfoForMail mailid) = do
    runQuery_ . sqlSelect "kontra_info_for_mails" $ do
      sqlWhereEq "mail_id" mailid
      sqlResult "mail_type"
      sqlResult "document_id"
      sqlResult "signatory_link_id"
    fetchMaybe fetchKontraInfoForMail

fetchKontraInfoForMail :: (KontraInfoForMailType, Maybe DocumentID, Maybe SignatoryLinkID) -> KontraInfoForMail
fetchKontraInfoForMail (DocumentInvitationMailT, Just did, Just sig)  = DocumentInvitationMail did sig
fetchKontraInfoForMail (DocumentInvitationMailT, _, _)  = ($unexpectedError "Failed to fetch KontraInfoForMail (DocumentInvitationMailT)")
fetchKontraInfoForMail (OtherDocumentMailT, Just did, Nothing)  = OtherDocumentMail did
fetchKontraInfoForMail (OtherDocumentMailT, _, _)  = ($unexpectedError "Failed to fetch KontraInfoForMail (OtherDocumentMailT)")
