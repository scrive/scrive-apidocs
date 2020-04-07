module Mails.KontraInfoForMail (
    KontraInfoForMail(..)
  , AddKontraInfoForMail(..)
  , GetUnreadEvents(..)
  , GetServiceTestEvents(..)
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
  logValue (DocumentInvitationMail did slid) =
    object ["type" .= ("invitation" :: Text), identifier did, identifier slid]
  logValue (DocumentConfirmationMail did slid) =
    object ["type" .= ("confirmation" :: Text), identifier did, identifier slid]
  logValue (OtherDocumentMail did) = object ["type" .= ("other" :: Text), identifier did]

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
      _ -> throwM RangeError { reRange = [(1, 3)], reValue = n }

instance ToSQL KontraInfoForMailType where
  type PQDest KontraInfoForMailType = PQDest Int16
  toSQL DocumentInvitationMailT   = toSQL (1 :: Int16)
  toSQL OtherDocumentMailT        = toSQL (2 :: Int16)
  toSQL DocumentConfirmationMailT = toSQL (3 :: Int16)

data AddKontraInfoForMail = AddKontraInfoForMail MailID KontraInfoForMail
instance (MonadDB m, MonadThrow m) => DBUpdate m AddKontraInfoForMail Bool where
  dbUpdate (AddKontraInfoForMail mailid mfdi) = do
    runQuery01 . sqlInsert "kontra_info_for_mails" $ do
      sqlSet "mail_id" mailid
      case mfdi of
        (DocumentInvitationMail did slid) -> do
          sqlSet "mail_type"         DocumentInvitationMailT
          sqlSet "document_id"       did
          sqlSet "signatory_link_id" slid
        (DocumentConfirmationMail did slid) -> do
          sqlSet "mail_type"         DocumentConfirmationMailT
          sqlSet "document_id"       did
          sqlSet "signatory_link_id" slid
        (OtherDocumentMail did) -> do
          sqlSet "mail_type"   OtherDocumentMailT
          sqlSet "document_id" did

fetchKontraInfoForMail
  :: (Maybe KontraInfoForMailType, Maybe DocumentID, Maybe SignatoryLinkID)
  -> Maybe KontraInfoForMail
fetchKontraInfoForMail (Nothing, _, _) = Nothing
fetchKontraInfoForMail (Just DocumentInvitationMailT, Just did, Just sig) =
  Just $ DocumentInvitationMail did sig
fetchKontraInfoForMail (Just DocumentInvitationMailT, _, _) =
  unexpectedError "Failed to fetch KontraInfoForMail (DocumentInvitationMailT)"
fetchKontraInfoForMail (Just DocumentConfirmationMailT, Just did, Just sig) =
  Just $ DocumentConfirmationMail did sig
fetchKontraInfoForMail (Just DocumentConfirmationMailT, _, _) =
  unexpectedError "Failed to fetch KontraInfoForMail (DocumentConfirmationMailT)"
fetchKontraInfoForMail (Just OtherDocumentMailT, Just did, Nothing) =
  Just $ OtherDocumentMail did
fetchKontraInfoForMail (Just OtherDocumentMailT, _, _) =
  unexpectedError "Failed to fetch KontraInfoForMail (OtherDocumentMailT)"


fetchEvent
  :: ( EventID
     , MailID
     , Event
     , Maybe KontraInfoForMailType
     , Maybe DocumentID
     , Maybe SignatoryLinkID
     )
  -> (EventID, MailID, Event, Maybe KontraInfoForMail)
fetchEvent (eid, mid, e, mkifmt, mkifmdid, mkifmslid) = (eid, mid, e, mkifm)
  where mkifm = fetchKontraInfoForMail (mkifmt, mkifmdid, mkifmslid)


getUnreadEvents
  :: MonadDB m => Bool -> m [(EventID, MailID, Event, Maybe KontraInfoForMail)]
getUnreadEvents service_test = do
  runQuery_ . sqlSelect "mails m" $ do
    sqlResult "e.id"
    sqlResult "e.mail_id"
    sqlResult "e.event"
    sqlResult "kifm.mail_type"
    sqlResult "kifm.document_id"
    sqlResult "kifm.signatory_link_id"
    sqlJoinOn "mail_events e" "m.id = e.mail_id"
    sqlLeftJoinOn "kontra_info_for_mails kifm" "e.mail_id = kifm.mail_id"
    sqlWhereEq "m.service_test" service_test
    sqlWhere "e.event_read IS NULL"
    sqlOrderBy "document_id"
    sqlOrderBy "m.id"
    sqlOrderBy "e.id"
  fetchMany fetchEvent

data GetUnreadEvents = GetUnreadEvents
instance MonadDB m => DBQuery m GetUnreadEvents [(EventID, MailID, Event, Maybe KontraInfoForMail)] where
  dbQuery GetUnreadEvents = getUnreadEvents False

data GetServiceTestEvents = GetServiceTestEvents
instance MonadDB m => DBQuery m GetServiceTestEvents [(EventID, MailID, Event, Maybe KontraInfoForMail)] where
  dbQuery GetServiceTestEvents = getUnreadEvents True
