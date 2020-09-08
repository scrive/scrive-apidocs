module Mails.KontraInfoForMail
  ( KontraInfoForMail(..)
  , AddKontraInfoForMail(..)
  , getUnreadMailEvents
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
import Mails.FromKontra.Tables
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
  deriving (Eq, Ord, Show, Generic)

-- For logging
instance ToJSON KontraInfoForMail

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

data AddKontraInfoForMail = AddKontraInfoForMail MailID [KontraInfoForMail]
instance (MonadDB m, MonadThrow m) => DBUpdate m AddKontraInfoForMail Bool where
  dbUpdate (AddKontraInfoForMail _      []  ) = pure True
  dbUpdate (AddKontraInfoForMail mailid kifm) = do
    n <- runQuery . sqlInsert "kontra_info_for_mails" $ do
      let values = kontraInfoToTuple <$> kifm
      sqlSet "mail_id" mailid
      sqlSetList "mail_type"         (view _1 <$> values)
      sqlSetList "document_id"       (view _2 <$> values)
      sqlSetList "signatory_link_id" (view _3 <$> values)
    pure $ n == length kifm
    where
      kontraInfoToTuple kifm' = case kifm' of
        (DocumentInvitationMail did slid) -> (DocumentInvitationMailT, did, Just slid)
        (DocumentConfirmationMail did slid) ->
          (DocumentConfirmationMailT, did, Just slid)
        (OtherDocumentMail did) -> (OtherDocumentMailT, did, Nothing)

data KontraInfoForMailAggregate
  = KontraInfoForMailAggregate
  { kontraInfoForMailType :: KontraInfoForMailType
  , kontraInfoForMailDocumentId :: DocumentID
  , kontraInfoForMailSignatoryLinkId :: Maybe SignatoryLinkID
  }

type instance CompositeRow KontraInfoForMailAggregate
  = (KontraInfoForMailType, DocumentID, Maybe SignatoryLinkID)

instance PQFormat KontraInfoForMailAggregate where
  pqFormat = compositeTypePqFormat ctKontraForMailAggregate

instance CompositeFromSQL KontraInfoForMailAggregate where
  toComposite (mail_type, did, mslid) = KontraInfoForMailAggregate
    { kontraInfoForMailType            = mail_type
    , kontraInfoForMailDocumentId      = did
    , kontraInfoForMailSignatoryLinkId = mslid
    }

fetchKontraInfoForMail :: KontraInfoForMailAggregate -> KontraInfoForMail
fetchKontraInfoForMail KontraInfoForMailAggregate { kontraInfoForMailType = mail_type, kontraInfoForMailDocumentId = did, kontraInfoForMailSignatoryLinkId = mslid }
  = mapper mslid
  where
    mapper = case mail_type of
      DocumentInvitationMailT -> maybe
        (unexpectedError "Failed to fetch KontraInfoForMail (DocumentInvitationMailT)")
        (DocumentInvitationMail did)
      DocumentConfirmationMailT -> maybe
        (unexpectedError "Failed to fetch KontraInfoForMail (DocumentConfirmationMailT)")
        (DocumentConfirmationMail did)
      OtherDocumentMailT -> maybe
        (OtherDocumentMail did)
        (const $ unexpectedError "Failed to fetch KontraInfoForMail (OtherDocumentMailT)")

fetchEvent
  :: (EventID, MailID, Event, CompositeArray1 KontraInfoForMailAggregate)
  -> (EventID, MailID, Event, [KontraInfoForMail])
fetchEvent (eid, mid, e, kifms) = (eid, mid, e, mkifm)
  where mkifm = fetchKontraInfoForMail <$> unCompositeArray1 kifms

getUnreadMailEvents
  :: MonadDB m => Bool -> m [(EventID, MailID, Event, [KontraInfoForMail])]
getUnreadMailEvents service_test = do
  runQuery_ . sqlSelect "mails m" $ do
    sqlResult "e.id"
    sqlResult "e.mail_id"
    sqlResult "e.event"
    sqlResult . mkSQL $ T.concat
      [ "ARRAY_AGG((kifm.mail_type, kifm.document_id, kifm.signatory_link_id)::"
      , unRawSQL $ ctName ctKontraForMailAggregate
      , ")"
      ]
    sqlJoinOn "mail_events e" "m.id = e.mail_id"
    sqlLeftJoinOn "kontra_info_for_mails kifm" "e.mail_id = kifm.mail_id"
    sqlWhereEq "m.service_test" service_test
    sqlWhere "e.event_read IS NULL"
    sqlOrderBy "m.id"
    sqlOrderBy "e.id"
    sqlGroupBy "m.id"
    sqlGroupBy "e.id"
  fetchMany fetchEvent
