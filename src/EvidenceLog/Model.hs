module EvidenceLog.Model (
    EvidenceEventType(..)
  , eventTextTemplateName  
  , apiActor
  , InsertEvidenceEvent(..)
  , GetEvidenceLog(..)
  , DocumentEvidenceEvent(..)
  , copyEvidenceLogToNewDocument
  , htmlDocFromEvidenceLog
  ) where

import DB
import Doc.DocStateData
import EvidenceLog.Tables
import IPAddress
import MinutesTime
import Misc
import Data.Typeable
import User.Model
import Util.Actor
import Version
import Templates.Templates
import qualified Templates.Fields as F
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

data InsertEvidenceEvent = InsertEvidenceEvent
                           EvidenceEventType      -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           (Maybe DocumentID)     -- The documentid if this event is about a document
                           Actor                  -- Actor
    deriving (Typeable)

eventTextTemplateName :: EvidenceEventType -> String
eventTextTemplateName e =  (show e) ++ "Text"

instance (MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEvent Bool where
  update (InsertEvidenceEvent event textFields mdid actor) = do
   text <- lift $ renderTemplateI (eventTextTemplateName event) $ textFields
   kRun01 $ mkSQL INSERT tableEvidenceLog [
      sql "document_id" mdid
    , sql "time" $ actorTime actor
    , sql "text" text
    , sql "event_type" event
    , sql "version_id" versionID
    , sql "user_id" $ actorUserID actor
    , sql "email" $ actorEmail actor
    , sql "request_ip_v4" $ actorIP actor
    , sql "signatory_link_id" $ actorSigLinkID actor
    , sql "api_user" $ actorAPIString actor
    ]

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
  }
  deriving (Eq, Ord, Show, Typeable)

htmlDocFromEvidenceLog :: TemplatesMonad m => String -> [DocumentEvidenceEvent] -> m String
htmlDocFromEvidenceLog title elog = do
  renderTemplate "htmlevidencelog" $ do
    F.value "documenttitle" title
    F.objects "entries" $ for elog $ \entry -> do
      F.value "time" $ formatMinutesTimeUTC (evTime entry) ++ " UTC"
      F.value "ip"   $ show <$> evIP4 entry
      F.value "text" $ evText entry

data GetEvidenceLog = GetEvidenceLog DocumentID
instance MonadDB m => DBQuery m GetEvidenceLog [DocumentEvidenceEvent] where
  query (GetEvidenceLog docid) = do
    _ <- kRun $ SQL ("SELECT "
      ++ "  document_id"
      ++ ", time"
      ++ ", text"
      ++ ", event_type"
      ++ ", version_id"
      ++ ", user_id"
      ++ ", email"
      ++ ", request_ip_v4"
      ++ ", request_ip_v6"
      ++ ", signatory_link_id"
      ++ ", api_user"
      ++ "  FROM evidence_log "
      ++ "  WHERE document_id = ?"
      ++ "  ORDER BY time DESC, id DESC") [
        toSql docid
      ]
    foldDB fetchEvidenceLog []
    where
      fetchEvidenceLog acc did' tm txt tp vid uid eml ip4 ip6 slid api =
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
          } : acc

copyEvidenceLogToNewDocument :: MonadDB m => DocumentID -> DocumentID -> DBEnv m ()
copyEvidenceLogToNewDocument fromdoc todoc = do
  _ <- kRun $ SQL ("INSERT INTO evidence_log ("
    ++ "  document_id"
    ++ ", time"
    ++ ", text"
    ++ ", event_type"
    ++ ", version_id"
    ++ ", user_id"
    ++ ", email"
    ++ ", request_ip_v4"
    ++ ", request_ip_v6"
    ++ ", signatory_link_id"
    ++ ", api_user"
    ++ ") SELECT "
    ++ "  ?"
    ++ ", time"
    ++ ", text"
    ++ ", event_type"
    ++ ", version_id"
    ++ ", user_id"
    ++ ", email"
    ++ ", request_ip_v4"
    ++ ", request_ip_v6"
    ++ ", signatory_link_id"
    ++ ", api_user"
    ++ " FROM evidence_log "
    ++ " WHERE document_id = ?") [
      toSql todoc
    , toSql fromdoc
    ]
  return ()

-- | A machine-readable event code for different types of events.
data EvidenceEventType =
  AddSigAttachmentEvidence                        |
  RemoveSigAttachmentsEvidence                    |
  RemoveDocumentAttachmentEvidence                |
  AddDocumentAttachmentEvidence                   |
  PendingToAwaitingAuthorEvidence                 |
  UpdateFieldsEvidence                            |
  SetElegitimationIdentificationEvidence          |
  SetEmailIdentificationEvidence                  |
  TimeoutDocumentEvidence                         |
  SignDocumentEvidence                            |
  SetInvitationDeliveryStatusEvidence             |
  SetDocumentUIEvidence                           |
  SetDocumentLocaleEvidence                       |
  SetDocumentTitleEvidence                        |
  SetDocumentAdvancedFunctionalityEvidence        |
  RemoveDaysToSignEvidence                        |
  SetDaysToSignEvidence                           |
  SetInvitationTextEvidence                       |
  RemoveSignatoryUserEvidence                     |
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
  SetDocumentTimeoutTimeEvidence                  |
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
  RemovedFromPadDevice
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
  safeConvert SetDocumentLocaleEvidence                       = return 13
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
    safeConvert 13 = return SetDocumentLocaleEvidence
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
    safeConvert s  = Left ConvertError { convSourceValue = show s
                                       , convSourceType = "Int"
                                       , convDestType = "EvidenceEventType"
                                       , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                       }

instance Convertible EvidenceEventType SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue EvidenceEventType where
  safeConvert s = safeConvert (fromSql s :: Int)
