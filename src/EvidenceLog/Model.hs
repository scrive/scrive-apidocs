module EvidenceLog.Model (
    EvidenceEventType(..)
  , eventTextTemplateName
  , apiActor
  , InsertEvidenceEvent(..)
  , InsertEvidenceEventWithAffectedSignatoryAndMsg(..)
  , InsertEvidenceEventForManyDocuments(..)
  , GetEvidenceLog(..)
  , DocumentEvidenceEvent(..)
  , copyEvidenceLogToNewDocument
  , copyEvidenceLogToNewDocuments
  ) where

import DB
import DB.SQL2
import IPAddress
import MinutesTime
import Data.Typeable
import User.Model
import Util.Actor
import Doc.SignatoryLinkID
import Version
import Doc.DocumentID
import Templates.Templates
import qualified Templates.Fields as F
import Control.Monad.Identity
import Control.Monad.Trans


data InsertEvidenceEventWithAffectedSignatoryAndMsg = InsertEvidenceEventWithAffectedSignatoryAndMsg
                           EvidenceEventType      -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           (Maybe DocumentID)     -- The documentid if this event is about a document
                           (Maybe SignatoryLinkID) -- Affected signatory
                           (Maybe String)          -- Message text
                           Actor                  -- Actor
    deriving (Typeable)

data InsertEvidenceEvent = InsertEvidenceEvent
                           EvidenceEventType      -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           (Maybe DocumentID)     -- The documentid if this event is about a document
                           Actor                  -- Actor
    deriving (Typeable)

data InsertEvidenceEventForManyDocuments = InsertEvidenceEventForManyDocuments
                           EvidenceEventType      -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           [DocumentID]           -- The list of document ids this event is about
                           Actor                  -- Actor
    deriving (Typeable)

eventTextTemplateName :: EvidenceEventType -> String
eventTextTemplateName e =  (show e) ++ "Text"

instance (MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsg Bool where
  update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields mdid maslid mmsg actor) = do
   text <- lift $ renderTemplateI (eventTextTemplateName event) $ textFields
   kRun01 $ sqlInsert "evidence_log" $ do
      sqlSet "document_id" mdid
      sqlSet "time" $ actorTime actor
      sqlSet "text" text
      sqlSet "event_type" event
      sqlSet "version_id" versionID
      sqlSet "user_id" $ actorUserID actor
      sqlSet "email" $ actorEmail actor
      sqlSet "request_ip_v4" $ actorIP actor
      sqlSet "signatory_link_id" $ actorSigLinkID actor
      sqlSet "api_user" $ actorAPIString actor
      sqlSet "affected_signatory_link_id" $ maslid
      sqlSet "message_text" $ mmsg
            
instance (MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEvent Bool where
  update (InsertEvidenceEvent event textFields mdid actor) = update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields mdid Nothing Nothing actor)
      
instance (MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventForManyDocuments () where
  update (InsertEvidenceEventForManyDocuments event textFields dids actor) = do
   texts <- lift $ forM dids $ \did -> renderTemplateI (eventTextTemplateName event) $ textFields >> F.value "did" (show did)
   kRun_ $ sqlInsert "evidence_log" $ do
      sqlSetList "document_id" dids
      sqlSet "time" $ actorTime actor
      sqlSetList "text" texts
      sqlSet "event_type" event
      sqlSet "version_id" versionID
      sqlSet "user_id" $ actorUserID actor
      sqlSet "email" $ actorEmail actor
      sqlSet "request_ip_v4" $ actorIP actor
      sqlSet "signatory_link_id" $ actorSigLinkID actor
      sqlSet "api_user" $ actorAPIString actor

data DocumentEvidenceEvent = DocumentEvidenceEvent {
    evDocumentID :: DocumentID
  , evTime       :: MinutesTime
  , evText       :: String
  , evType       :: EvidenceEventType
  , evVersionID  :: String
  , evEmail      :: Maybe String
  , evUserID     :: Maybe UserID
  , evIP4        :: Maybe IPAddress
  , evIP6        :: Maybe IPAddress
  , evSigLinkID  :: Maybe SignatoryLinkID
  , evAPI        :: Maybe String
  , evAffectedSigLinkID :: Maybe SignatoryLinkID -- Some events affect only one signatory, but actor is out system or author. We express it here, since we can't with evType.
  , evMessageText :: Maybe String -- Some events have message connected to them (like reminders). We don't store such events in documents, but they should not get lost.
  }
  deriving (Eq, Ord, Show, Typeable)

data GetEvidenceLog = GetEvidenceLog DocumentID
instance MonadDB m => DBQuery m GetEvidenceLog [DocumentEvidenceEvent] where
  query (GetEvidenceLog docid) = do
    _ <- kRun $ SQL ("SELECT "
      <> "  document_id"
      <> ", time"
      <> ", text"
      <> ", event_type"
      <> ", version_id"
      <> ", user_id"
      <> ", email"
      <> ", request_ip_v4"
      <> ", request_ip_v6"
      <> ", signatory_link_id"
      <> ", api_user"
      <> ", affected_signatory_link_id"
      <> ", message_text"
      <> "  FROM evidence_log "
      <> "  WHERE document_id = ?"
      <> "  ORDER BY time DESC, id DESC") [
        toSql docid
      ]
    kFold fetchEvidenceLog []
    where
      fetchEvidenceLog acc did' tm txt tp vid uid eml ip4 ip6 slid api aslid emsg =
        DocumentEvidenceEvent {
            evDocumentID = did'
          , evTime       = tm
          , evText       = txt
          , evType       = tp
          , evVersionID  = vid
          , evUserID     = uid
          , evEmail      = eml
          , evIP4        = ip4
          , evIP6        = ip6
          , evSigLinkID  = slid
          , evAPI        = api
          , evAffectedSigLinkID = aslid
          , evMessageText = emsg
          } : acc

copyEvidenceLogToNewDocument :: MonadDB m => DocumentID -> DocumentID -> DBEnv m ()
copyEvidenceLogToNewDocument fromdoc todoc = do
  copyEvidenceLogToNewDocuments fromdoc [todoc]

copyEvidenceLogToNewDocuments :: MonadDB m => DocumentID -> [DocumentID] -> DBEnv m ()
copyEvidenceLogToNewDocuments fromdoc todocs = do
  kRun_ $ "INSERT INTO evidence_log ("
    <> "  document_id"
    <> ", time"
    <> ", text"
    <> ", event_type"
    <> ", version_id"
    <> ", user_id"
    <> ", email"
    <> ", request_ip_v4"
    <> ", request_ip_v6"
    <> ", signatory_link_id"
    <> ", api_user"
    <> ", affected_signatory_link_id"
    <> ", message_text"
    <> ") SELECT "
    <> "  todocs.id :: BIGINT"
    <> ", time"
    <> ", text"
    <> ", event_type"
    <> ", version_id"
    <> ", user_id"
    <> ", email"
    <> ", request_ip_v4"
    <> ", request_ip_v6"
    <> ", signatory_link_id"
    <> ", api_user"
    <> ", affected_signatory_link_id"
    <> ", message_text"
    <> " FROM evidence_log, (VALUES" <+> sqlConcatComma (map (parenthesize . sqlParam) todocs) <+> ") AS todocs(id)"
    <> " WHERE evidence_log.document_id =" <?> fromdoc

-- | A machine-readable event code for different types of events.
data EvidenceEventType =
  AddSigAttachmentEvidence                        |
  RemoveSigAttachmentsEvidence                    |
  RemoveDocumentAttachmentEvidence                |
  AddDocumentAttachmentEvidence                   |
  PendingToAwaitingAuthorEvidence                 |
  UpdateFieldsEvidence                            | -- not used anymore
  SetElegitimationIdentificationEvidence          | -- not used anymore
  SetEmailIdentificationEvidence                  | -- not used anymore
  TimeoutDocumentEvidence                         |
  SignDocumentEvidence                            |
  SetInvitationDeliveryStatusEvidence             |
  SetDocumentUIEvidence                           |
  SetDocumentLangEvidence                         |
  SetDocumentTitleEvidence                        |
  SetDocumentAdvancedFunctionalityEvidence        |
  RemoveDaysToSignEvidence                        | -- not used anymore
  SetDaysToSignEvidence                           |
  SetInvitationTextEvidence                       |
  RemoveSignatoryUserEvidence                     | -- not used anymore
  SetSignatoryUserEvidence                        |
  RemoveSignatoryCompanyEvidence                  |
  SetSignatoryCompanyEvidence                     |
  SetDocumentTagsEvidence                         |
  SaveSigAttachmentEvidence                       |
  SaveDocumentForUserEvidence                     |
  RestartDocumentEvidence                         |
  ReallyDeleteDocumentEvidence                    |
  NewDocumentEvidence                             |
  MarkInvitationReadEvidence                      |
  CloseDocumentEvidence                           |
  ChangeSignatoryEmailWhenUndeliveredEvidence     |
  ChangeMainfileEvidence                          |
  CancelDocumenElegEvidence                       |
  CancelDocumentEvidence                          |
  AttachFileEvidence                              |
  AttachSealedFileEvidence                        |
  PreparationToPendingEvidence                    |
  DeleteSigAttachmentEvidence                     |
  AuthorUsesCSVEvidence                           |
  ErrorDocumentEvidence                           |
  MarkDocumentSeenEvidence                        |
  RejectDocumentEvidence                          |
  SetDocumentInviteTimeEvidence                   |
  SetDocumentTimeoutTimeEvidence                  | -- not used anymore
  RestoreArchivedDocumentEvidence                 |
  InvitationEvidence                              |
  SignableFromDocumentIDWithUpdatedAuthorEvidence |
  ArchiveDocumentEvidence                         |
  ResetSignatoryDetailsEvidence                   |
  AdminOnlySaveForUserEvidence                    |
  SignableFromDocumentEvidence                    |
  TemplateFromDocumentEvidence                    |
  AttachCSVUploadEvidence                         |
  SendToPadDevice                                 |
  RemovedFromPadDevice                            |
  AddSignatoryEvidence                            |
  RemoveSignatoryEvidence                         |
  AddFieldEvidence                                |
  RemoveFieldEvidence                             |
  ChangeFieldEvidence                             |
  ResealedPDF                                     |
  OldDocumentHistory                              |
  SetStandardAuthenticationMethodEvidence         |
  SetELegAuthenticationMethodEvidence             |
  SetEmailDeliveryMethodEvidence                  |
  SetPadDeliveryMethodEvidence                    |
  SetAPIDeliveryMethodEvidence                    |
  ReminderSend                                    |  --Renamed
  SetDocumentProcessEvidence                      |
  DetachFileEvidence                              |
  InvitationDelivered                             |
  InvitationUndelivered                           |
  SignatoryLinkVisited
  deriving (Eq, Show, Read, Ord)

instance Convertible EvidenceEventType Int where
  safeConvert AddSigAttachmentEvidence                        = return 1
  safeConvert RemoveSigAttachmentsEvidence                    = return 2
  safeConvert RemoveDocumentAttachmentEvidence                = return 3
  safeConvert AddDocumentAttachmentEvidence                   = return 4
  safeConvert PendingToAwaitingAuthorEvidence                 = return 5
  safeConvert UpdateFieldsEvidence                            = return 6
  safeConvert SetElegitimationIdentificationEvidence          = return 7
  safeConvert SetEmailIdentificationEvidence                  = return 8
  safeConvert TimeoutDocumentEvidence                         = return 9
  safeConvert SignDocumentEvidence                            = return 10
  safeConvert SetInvitationDeliveryStatusEvidence             = return 11
  safeConvert SetDocumentUIEvidence                           = return 12
  safeConvert SetDocumentLangEvidence                         = return 13
  safeConvert SetDocumentTitleEvidence                        = return 14
  safeConvert SetDocumentAdvancedFunctionalityEvidence        = return 15
  safeConvert RemoveDaysToSignEvidence                        = return 16
  safeConvert SetDaysToSignEvidence                           = return 17
  safeConvert SetInvitationTextEvidence                       = return 18
  safeConvert RemoveSignatoryUserEvidence                     = return 19
  safeConvert SetSignatoryUserEvidence                        = return 20
  safeConvert RemoveSignatoryCompanyEvidence                  = return 21
  safeConvert SetSignatoryCompanyEvidence                     = return 22
  safeConvert SetDocumentTagsEvidence                         = return 23
  safeConvert SaveSigAttachmentEvidence                       = return 24
  safeConvert SaveDocumentForUserEvidence                     = return 25
  safeConvert RestartDocumentEvidence                         = return 26
  safeConvert ReallyDeleteDocumentEvidence                    = return 27
  safeConvert NewDocumentEvidence                             = return 28
  safeConvert MarkInvitationReadEvidence                      = return 29
  safeConvert CloseDocumentEvidence                           = return 30
  safeConvert ChangeSignatoryEmailWhenUndeliveredEvidence     = return 31
  safeConvert ChangeMainfileEvidence                          = return 32
  safeConvert CancelDocumenElegEvidence                       = return 33
  safeConvert CancelDocumentEvidence                          = return 34
  safeConvert AttachFileEvidence                              = return 35
  safeConvert AttachSealedFileEvidence                        = return 36
  safeConvert PreparationToPendingEvidence                    = return 37
  safeConvert DeleteSigAttachmentEvidence                     = return 38
  safeConvert AuthorUsesCSVEvidence                           = return 39
  safeConvert ErrorDocumentEvidence                           = return 40
  safeConvert MarkDocumentSeenEvidence                        = return 41
  safeConvert RejectDocumentEvidence                          = return 42
  safeConvert SetDocumentInviteTimeEvidence                   = return 43
  safeConvert SetDocumentTimeoutTimeEvidence                  = return 44
  safeConvert RestoreArchivedDocumentEvidence                 = return 45
  safeConvert InvitationEvidence                              = return 46
  safeConvert SignableFromDocumentIDWithUpdatedAuthorEvidence = return 47
  safeConvert ArchiveDocumentEvidence                         = return 48
  safeConvert ResetSignatoryDetailsEvidence                   = return 49
  safeConvert AdminOnlySaveForUserEvidence                    = return 50
  safeConvert SignableFromDocumentEvidence                    = return 51
  safeConvert TemplateFromDocumentEvidence                    = return 52
  safeConvert AttachCSVUploadEvidence                         = return 53
  safeConvert SendToPadDevice                                 = return 54
  safeConvert RemovedFromPadDevice                            = return 55
  safeConvert AddSignatoryEvidence                            = return 56
  safeConvert RemoveSignatoryEvidence                         = return 57
  safeConvert AddFieldEvidence                                = return 58
  safeConvert RemoveFieldEvidence                             = return 59
  safeConvert ChangeFieldEvidence                             = return 60
  safeConvert ResealedPDF                                     = return 61
  safeConvert OldDocumentHistory                              = return 62
  safeConvert SetStandardAuthenticationMethodEvidence         = return 63
  safeConvert SetELegAuthenticationMethodEvidence             = return 64
  safeConvert SetEmailDeliveryMethodEvidence                  = return 65
  safeConvert SetPadDeliveryMethodEvidence                    = return 66
  safeConvert SetAPIDeliveryMethodEvidence                    = return 67
  safeConvert ReminderSend                                    = return 68
  safeConvert SetDocumentProcessEvidence                      = return 69
  safeConvert DetachFileEvidence                              = return 70
  safeConvert InvitationDelivered                             = return 71
  safeConvert InvitationUndelivered                           = return 72
  safeConvert SignatoryLinkVisited                            = return 73

instance Convertible Int EvidenceEventType where
    safeConvert 1  = return AddSigAttachmentEvidence
    safeConvert 2  = return RemoveSigAttachmentsEvidence
    safeConvert 3  = return RemoveDocumentAttachmentEvidence
    safeConvert 4  = return AddDocumentAttachmentEvidence
    safeConvert 5  = return PendingToAwaitingAuthorEvidence
    safeConvert 6  = return UpdateFieldsEvidence
    safeConvert 7  = return SetElegitimationIdentificationEvidence
    safeConvert 8  = return SetEmailIdentificationEvidence
    safeConvert 9  = return TimeoutDocumentEvidence
    safeConvert 10 = return SignDocumentEvidence
    safeConvert 11 = return SetInvitationDeliveryStatusEvidence
    safeConvert 12 = return SetDocumentUIEvidence
    safeConvert 13 = return SetDocumentLangEvidence
    safeConvert 14 = return SetDocumentTitleEvidence
    safeConvert 15 = return SetDocumentAdvancedFunctionalityEvidence
    safeConvert 16 = return RemoveDaysToSignEvidence
    safeConvert 17 = return SetDaysToSignEvidence
    safeConvert 18 = return SetInvitationTextEvidence
    safeConvert 19 = return RemoveSignatoryUserEvidence
    safeConvert 20 = return SetSignatoryUserEvidence
    safeConvert 21 = return RemoveSignatoryCompanyEvidence
    safeConvert 22 = return SetSignatoryCompanyEvidence
    safeConvert 23 = return SetDocumentTagsEvidence
    safeConvert 24 = return SaveSigAttachmentEvidence
    safeConvert 25 = return SaveDocumentForUserEvidence
    safeConvert 26 = return RestartDocumentEvidence
    safeConvert 27 = return ReallyDeleteDocumentEvidence
    safeConvert 28 = return NewDocumentEvidence
    safeConvert 29 = return MarkInvitationReadEvidence
    safeConvert 30 = return CloseDocumentEvidence
    safeConvert 31 = return ChangeSignatoryEmailWhenUndeliveredEvidence
    safeConvert 32 = return ChangeMainfileEvidence
    safeConvert 33 = return CancelDocumenElegEvidence
    safeConvert 34 = return CancelDocumentEvidence
    safeConvert 35 = return AttachFileEvidence
    safeConvert 36 = return AttachSealedFileEvidence
    safeConvert 37 = return PreparationToPendingEvidence
    safeConvert 38 = return DeleteSigAttachmentEvidence
    safeConvert 39 = return AuthorUsesCSVEvidence
    safeConvert 40 = return ErrorDocumentEvidence
    safeConvert 41 = return MarkDocumentSeenEvidence
    safeConvert 42 = return RejectDocumentEvidence
    safeConvert 43 = return SetDocumentInviteTimeEvidence
    safeConvert 44 = return SetDocumentTimeoutTimeEvidence
    safeConvert 45 = return RestoreArchivedDocumentEvidence
    safeConvert 46 = return InvitationEvidence
    safeConvert 47 = return SignableFromDocumentIDWithUpdatedAuthorEvidence
    safeConvert 48 = return ArchiveDocumentEvidence
    safeConvert 49 = return ResetSignatoryDetailsEvidence
    safeConvert 50 = return AdminOnlySaveForUserEvidence
    safeConvert 51 = return SignableFromDocumentEvidence
    safeConvert 52 = return TemplateFromDocumentEvidence
    safeConvert 53 = return AttachCSVUploadEvidence
    safeConvert 54 = return SendToPadDevice
    safeConvert 55 = return RemovedFromPadDevice
    safeConvert 56 = return AddSignatoryEvidence
    safeConvert 57 = return RemoveSignatoryEvidence
    safeConvert 58 = return AddFieldEvidence
    safeConvert 59 = return RemoveFieldEvidence
    safeConvert 60 = return ChangeFieldEvidence
    safeConvert 61 = return ResealedPDF
    safeConvert 62 = return OldDocumentHistory
    safeConvert 63 = return SetStandardAuthenticationMethodEvidence
    safeConvert 64 = return SetELegAuthenticationMethodEvidence
    safeConvert 65 = return SetEmailDeliveryMethodEvidence
    safeConvert 66 = return SetPadDeliveryMethodEvidence
    safeConvert 67 = return SetAPIDeliveryMethodEvidence
    safeConvert 68 = return ReminderSend
    safeConvert 69 = return SetDocumentProcessEvidence
    safeConvert 70 = return DetachFileEvidence
    safeConvert 71 = return InvitationDelivered
    safeConvert 72 = return InvitationUndelivered
    safeConvert 73 = return SignatoryLinkVisited
    safeConvert s  = Left ConvertError { convSourceValue = show s
                                       , convSourceType = "Int"
                                       , convDestType = "EvidenceEventType"
                                       , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                       }

instance Convertible EvidenceEventType SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue EvidenceEventType where
  safeConvert s = safeConvert (fromSql s :: Int)
