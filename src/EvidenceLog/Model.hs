module EvidenceLog.Model (
    EvidenceEventType(..)
  , CurrentEvidenceEventType(..)
  , ObsoleteEvidenceEventType(..)
  , eventTextTemplateName
  , apiActor
  , InsertEvidenceEvent(..)
  , InsertEvidenceEventWithAffectedSignatoryAndMsg(..)
  , GetEvidenceLog(..)
  , DocumentEvidenceEvent
  , DocumentEvidenceEventWithSignatoryLink
  , DocumentEvidenceEvent'(..)
  , evidenceLogText
  , copyEvidenceLogToNewDocument
  , copyEvidenceLogToNewDocuments
  , signatoryLinkTemplateFields
  ) where

import Control.Applicative ((<$>), (<*>))
import DB
import DB.SQL2
import qualified HostClock.Model as HC
import IPAddress
import MinutesTime
import Data.Typeable
import User.Model
import Util.Actor
import Util.HasSomeUserInfo (getIdentifier, getEmail)
import Doc.SignatoryLinkID
import Doc.DocStateData (SignatoryLink(..), AuthenticationMethod(..), DeliveryMethod(..))
import Version
import Doc.DocumentID
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F
import Control.Monad.Identity

data InsertEvidenceEventWithAffectedSignatoryAndMsg = InsertEvidenceEventWithAffectedSignatoryAndMsg
                           CurrentEvidenceEventType -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           (Maybe SignatoryLink)  -- Affected signatory
                           (Maybe String)         -- Message text
                           Actor                  -- Actor
                           DocumentID             -- Affected document
    deriving (Typeable)

data InsertEvidenceEvent = InsertEvidenceEvent
                           CurrentEvidenceEventType -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           Actor                  -- Actor
                           DocumentID             -- Affected document
    deriving (Typeable)

eventTextTemplateName :: CurrentEvidenceEventType -> String
eventTextTemplateName e =  show e ++ "Text"

signatoryLinkTemplateFields :: Monad m => SignatoryLink -> F.Fields m ()
signatoryLinkTemplateFields sl = do
  F.value "identified"  $ signatorylinkauthenticationmethod sl == ELegAuthentication
                       || not (signatoryisauthor sl || signatorylinkdeliverymethod sl == APIDelivery)
  F.value "eleg"        $ signatorylinkauthenticationmethod sl == ELegAuthentication
  F.value "api"         $ signatorylinkdeliverymethod sl == APIDelivery
  F.value "pad"         $ signatorylinkdeliverymethod sl == PadDelivery
  F.value "email"       $ signatorylinkdeliverymethod sl == EmailDelivery
  F.value "mobile"      $ signatorylinkdeliverymethod sl == MobileDelivery
  F.value "emailmobile" $ signatorylinkdeliverymethod sl == EmailAndMobileDelivery
  F.value "viewing"     $ not $ signatoryispartner sl
  F.value "signing"     $ signatoryispartner sl

-- | Create evidence text that goes into evidence log
evidenceLogText :: TemplatesMonad m => CurrentEvidenceEventType -> F.Fields Identity () -> Maybe SignatoryLink -> Maybe String -> Actor -> m String
evidenceLogText event textFields masl mmsg actor = do
   let fields = do
         F.value "full" True
         F.value "actor" $ actorWho actor
         maybe (return ()) (F.value "text") mmsg
         case masl of
           Nothing -> return ()
           Just sl -> do
             F.value "signatory" $ getIdentifier sl
             F.value "signatory_email" $ getEmail sl
             signatoryLinkTemplateFields sl
         textFields
   ts <- getTextTemplatesByLanguage $ codeFromLang LANG_EN
   return $ runIdentity $ renderHelper ts (eventTextTemplateName event) fields

instance (MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsg Bool where
  update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields masl mmsg actor did) = do
   text <- evidenceLogText event textFields masl mmsg actor
   kRun01 $ sqlInsert "evidence_log" $ do
      sqlSet "document_id" did
      sqlSet "time" $ actorTime actor
      sqlSet "text" text
      sqlSet "event_type" (Current event)
      sqlSet "version_id" versionID
      sqlSet "user_id" $ actorUserID actor
      sqlSet "email" $ actorEmail actor
      sqlSet "request_ip_v4" $ actorIP actor
      sqlSet "signatory_link_id" $ actorSigLinkID actor
      sqlSet "api_user" $ actorAPIString actor
      sqlSet "affected_signatory_link_id" $ signatorylinkid <$> masl
      sqlSet "message_text" $ mmsg
      sqlSet "client_time" $ actorClientTime actor
      sqlSet "client_name" $ actorClientName actor

instance (MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEvent Bool where
  update (InsertEvidenceEvent event textFields actor did) = update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields Nothing Nothing actor did)

type DocumentEvidenceEvent = DocumentEvidenceEvent' SignatoryLinkID

-- | For generating evidence texts, we substitute full signatory links for their IDs
type DocumentEvidenceEventWithSignatoryLink = DocumentEvidenceEvent' SignatoryLink

data DocumentEvidenceEvent' s = DocumentEvidenceEvent {
    evDocumentID :: DocumentID
  , evTime       :: MinutesTime
  , evClientTime :: Maybe MinutesTime
  , evClientName :: Maybe String
  , evClockErrorEstimate :: Maybe HC.ClockErrorEstimate
  , evText       :: String
  , evType       :: EvidenceEventType
  , evVersionID  :: String
  , evEmail      :: Maybe String
  , evUserID     :: Maybe UserID
  , evIP4        :: Maybe IPAddress
  , evIP6        :: Maybe IPAddress
  , evSigLink    :: Maybe s
  , evAPI        :: Maybe String
  , evAffectedSigLink :: Maybe s -- Some events affect only one signatory, but actor is out system or author. We express it here, since we can't with evType.
  , evMessageText :: Maybe String -- Some events have message connected to them (like reminders). We don't store such events in documents, but they should not get lost.
  }
  deriving (Eq, Ord, Show, Typeable)

data GetEvidenceLog = GetEvidenceLog DocumentID
instance MonadDB m => DBQuery m GetEvidenceLog [DocumentEvidenceEvent] where
  query (GetEvidenceLog docid) = do
    _ <- kRun $ SQL ("SELECT "
      <> "  document_id"
      <> ", evidence_log.time"
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
      <> ", client_time"
      <> ", client_name"
      <> ", host_clock.time"
      <> ", host_clock.clock_offset"
      <> ", host_clock.clock_frequency"
      <> "  FROM evidence_log LEFT JOIN host_clock ON host_clock.time = (SELECT max(host_clock.time) FROM host_clock WHERE host_clock.time <= evidence_log.time)"
      <> "  WHERE document_id = ?"
      <> "  ORDER BY evidence_log.time DESC, id DESC") [
        toSql docid
      ]
    kFold fetchEvidenceLog []
    where
      fetchEvidenceLog acc did' tm txt tp vid uid eml ip4 ip6 slid api aslid emsg ctime cname hctime offset frequency =
        DocumentEvidenceEvent {
            evDocumentID = did'
          , evTime       = tm
          , evClientTime = ctime
          , evClientName = cname
          , evClockErrorEstimate = HC.ClockErrorEstimate <$> hctime <*> offset <*> frequency
          , evText       = txt
          , evType       = tp
          , evVersionID  = vid
          , evUserID     = uid
          , evEmail      = eml
          , evIP4        = ip4
          , evIP6        = ip6
          , evSigLink    = slid
          , evAPI        = api
          , evAffectedSigLink = aslid
          , evMessageText = emsg
          } : acc

copyEvidenceLogToNewDocument :: MonadDB m => DocumentID -> DocumentID -> m ()
copyEvidenceLogToNewDocument fromdoc todoc = do
  copyEvidenceLogToNewDocuments fromdoc [todoc]

copyEvidenceLogToNewDocuments :: MonadDB m => DocumentID -> [DocumentID] -> m ()
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
    <> ", client_time"
    <> ", client_name"
    <> ") (SELECT "
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
    <> ", client_time"
    <> ", client_name"
    <> " FROM evidence_log, (VALUES" <+> sqlConcatComma (map (parenthesize . sqlParam) todocs) <+> ") AS todocs(id)"
    <> " WHERE evidence_log.document_id =" <?> fromdoc <+> ") ORDER BY evidence_log.id"

-- | A machine-readable event code for different types of events.
data EvidenceEventType =
   Current CurrentEvidenceEventType
 | Obsolete ObsoleteEvidenceEventType
  deriving (Eq, Ord, Show)

data CurrentEvidenceEventType =
  TimeoutDocumentEvidence                         |
  SignDocumentEvidence                            |
  SaveSigAttachmentEvidence                       |
  RestartDocumentEvidence                         |
  MarkInvitationReadEvidence                      |
  CloseDocumentEvidence                           |
  ChangeSignatoryEmailWhenUndeliveredEvidence     |
  CancelDocumenElegEvidence                       |
  CancelDocumentEvidence                          |
  AttachSealedFileEvidence                        |
  PreparationToPendingEvidence                    |
  DeleteSigAttachmentEvidence                     |
  RejectDocumentEvidence                          |
  InvitationEvidence                              |
  SendToPadDevice                                 |
  RemovedFromPadDevice                            |
  ResealedPDF                                     |
  ReminderSend                                    |  --Renamed
  InvitationDeliveredByEmail                      |
  InvitationUndeliveredByEmail                    |
  SignatoryLinkVisited                            |
  ProlongDocumentEvidence                         |
  ChangeSignatoryPhoneWhenUndeliveredEvidence     |
  InvitationDeliveredBySMS                        |
  InvitationUndeliveredBySMS                      |
  AttachGuardtimeSealedFileEvidence               |
  AttachExtendedSealedFileEvidence                |
  ErrorSealingDocumentEvidence                    |
  AutomaticReminderSent
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

data ObsoleteEvidenceEventType =
  AddSigAttachmentEvidence                        |
  RemoveSigAttachmentsEvidence                    |
  RemoveDocumentAttachmentEvidence                |
  AddDocumentAttachmentEvidence                   |
  PendingToAwaitingAuthorEvidence                 |
  UpdateFieldsEvidence                            |
  SetElegitimationIdentificationEvidence          |
  SetEmailIdentificationEvidence                  |
  SetInvitationDeliveryStatusEvidence             |
  SetDocumentUIEvidence                           |
  SetDocumentLangEvidence                         |
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
  SaveDocumentForUserEvidence                     |
  ReallyDeleteDocumentEvidence                    |
  NewDocumentEvidence                             |
  ChangeMainfileEvidence                          |
  AttachFileEvidence                              |
  MarkDocumentSeenEvidence                        |
  SetDocumentTimeoutTimeEvidence                  |
  RestoreArchivedDocumentEvidence                 |
  SignableFromDocumentIDWithUpdatedAuthorEvidence |
  ArchiveDocumentEvidence                         |
  ResetSignatoryDetailsEvidence                   |
  AdminOnlySaveForUserEvidence                    |
  SignableFromDocumentEvidence                    |
  TemplateFromDocumentEvidence                    |
  AttachCSVUploadEvidence                         |
  AddSignatoryEvidence                            |
  RemoveSignatoryEvidence                         |
  AddFieldEvidence                                |
  RemoveFieldEvidence                             |
  ChangeFieldEvidence                             |
  OldDocumentHistory                              |
  SetEmailDeliveryMethodEvidence                  |
  SetPadDeliveryMethodEvidence                    |
  SetAPIDeliveryMethodEvidence                    |
  SetDocumentProcessEvidence                      |
  DetachFileEvidence                              |
  SetStandardAuthenticationMethodEvidence         |
  SetELegAuthenticationMethodEvidence             |
  AuthorUsesCSVEvidence                           |
  ErrorDocumentEvidence                           |
  SetDocumentInviteTimeEvidence
  deriving (Eq, Show, Read, Ord)

instance Convertible EvidenceEventType Int where
  safeConvert (Obsolete AddSigAttachmentEvidence)                        = return 1
  safeConvert (Obsolete RemoveSigAttachmentsEvidence)                    = return 2
  safeConvert (Obsolete RemoveDocumentAttachmentEvidence)                = return 3
  safeConvert (Obsolete AddDocumentAttachmentEvidence)                   = return 4
  safeConvert (Obsolete PendingToAwaitingAuthorEvidence)                 = return 5
  safeConvert (Obsolete UpdateFieldsEvidence)                            = return 6
  safeConvert (Obsolete SetElegitimationIdentificationEvidence)          = return 7
  safeConvert (Obsolete SetEmailIdentificationEvidence)                  = return 8
  safeConvert (Current TimeoutDocumentEvidence)                          = return 9
  safeConvert (Current SignDocumentEvidence)                             = return 10
  safeConvert (Obsolete SetInvitationDeliveryStatusEvidence)             = return 11
  safeConvert (Obsolete SetDocumentUIEvidence)                           = return 12
  safeConvert (Obsolete SetDocumentLangEvidence)                         = return 13
  safeConvert (Obsolete SetDocumentTitleEvidence)                        = return 14
  safeConvert (Obsolete SetDocumentAdvancedFunctionalityEvidence)        = return 15
  safeConvert (Obsolete RemoveDaysToSignEvidence)                        = return 16
  safeConvert (Obsolete SetDaysToSignEvidence)                           = return 17
  safeConvert (Obsolete SetInvitationTextEvidence)                       = return 18
  safeConvert (Obsolete RemoveSignatoryUserEvidence)                     = return 19
  safeConvert (Obsolete SetSignatoryUserEvidence)                        = return 20
  safeConvert (Obsolete RemoveSignatoryCompanyEvidence)                  = return 21
  safeConvert (Obsolete SetSignatoryCompanyEvidence)                     = return 22
  safeConvert (Obsolete SetDocumentTagsEvidence)                         = return 23
  safeConvert (Current SaveSigAttachmentEvidence)                        = return 24
  safeConvert (Obsolete SaveDocumentForUserEvidence)                     = return 25
  safeConvert (Current RestartDocumentEvidence)                          = return 26
  safeConvert (Obsolete ReallyDeleteDocumentEvidence)                    = return 27
  safeConvert (Obsolete NewDocumentEvidence)                             = return 28
  safeConvert (Current MarkInvitationReadEvidence)                       = return 29
  safeConvert (Current CloseDocumentEvidence)                            = return 30
  safeConvert (Current ChangeSignatoryEmailWhenUndeliveredEvidence)      = return 31
  safeConvert (Obsolete ChangeMainfileEvidence)                          = return 32
  safeConvert (Current CancelDocumenElegEvidence)                        = return 33
  safeConvert (Current CancelDocumentEvidence)                           = return 34
  safeConvert (Obsolete AttachFileEvidence)                              = return 35
  safeConvert (Current AttachSealedFileEvidence)                         = return 36
  safeConvert (Current PreparationToPendingEvidence)                     = return 37
  safeConvert (Current DeleteSigAttachmentEvidence)                      = return 38
  safeConvert (Obsolete AuthorUsesCSVEvidence)                           = return 39
  safeConvert (Obsolete ErrorDocumentEvidence)                           = return 40
  safeConvert (Obsolete MarkDocumentSeenEvidence)                        = return 41
  safeConvert (Current RejectDocumentEvidence)                           = return 42
  safeConvert (Obsolete SetDocumentInviteTimeEvidence)                   = return 43
  safeConvert (Obsolete SetDocumentTimeoutTimeEvidence)                  = return 44
  safeConvert (Obsolete RestoreArchivedDocumentEvidence)                 = return 45
  safeConvert (Current InvitationEvidence)                               = return 46
  safeConvert (Obsolete SignableFromDocumentIDWithUpdatedAuthorEvidence) = return 47
  safeConvert (Obsolete ArchiveDocumentEvidence)                         = return 48
  safeConvert (Obsolete ResetSignatoryDetailsEvidence)                   = return 49
  safeConvert (Obsolete AdminOnlySaveForUserEvidence)                    = return 50
  safeConvert (Obsolete SignableFromDocumentEvidence)                    = return 51
  safeConvert (Obsolete TemplateFromDocumentEvidence)                    = return 52
  safeConvert (Obsolete AttachCSVUploadEvidence)                         = return 53
  safeConvert (Current SendToPadDevice)                                  = return 54
  safeConvert (Current RemovedFromPadDevice)                             = return 55
  safeConvert (Obsolete AddSignatoryEvidence)                            = return 56
  safeConvert (Obsolete RemoveSignatoryEvidence)                         = return 57
  safeConvert (Obsolete AddFieldEvidence)                                = return 58
  safeConvert (Obsolete RemoveFieldEvidence)                             = return 59
  safeConvert (Obsolete ChangeFieldEvidence)                             = return 60
  safeConvert (Current ResealedPDF)                                      = return 61
  safeConvert (Obsolete OldDocumentHistory)                              = return 62
  safeConvert (Obsolete SetStandardAuthenticationMethodEvidence)         = return 63
  safeConvert (Obsolete SetELegAuthenticationMethodEvidence)             = return 64
  safeConvert (Obsolete SetEmailDeliveryMethodEvidence)                  = return 65
  safeConvert (Obsolete SetPadDeliveryMethodEvidence)                    = return 66
  safeConvert (Obsolete SetAPIDeliveryMethodEvidence)                    = return 67
  safeConvert (Current ReminderSend)                                     = return 68
  safeConvert (Obsolete SetDocumentProcessEvidence)                      = return 69
  safeConvert (Obsolete DetachFileEvidence)                              = return 70
  safeConvert (Current InvitationDeliveredByEmail)                       = return 71
  safeConvert (Current InvitationUndeliveredByEmail)                     = return 72
  safeConvert (Current SignatoryLinkVisited)                             = return 73
  safeConvert (Current ProlongDocumentEvidence)                          = return 74
  safeConvert (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)      = return 75
  safeConvert (Current InvitationDeliveredBySMS)                         = return 76
  safeConvert (Current InvitationUndeliveredBySMS)                       = return 77
  safeConvert (Current AttachGuardtimeSealedFileEvidence)                = return 78
  safeConvert (Current AttachExtendedSealedFileEvidence)                 = return 79
  safeConvert (Current ErrorSealingDocumentEvidence)                     = return 80
  safeConvert (Current AutomaticReminderSent)                            = return 81


instance Convertible Int EvidenceEventType where
    safeConvert 1  = return (Obsolete AddSigAttachmentEvidence)
    safeConvert 2  = return (Obsolete RemoveSigAttachmentsEvidence)
    safeConvert 3  = return (Obsolete RemoveDocumentAttachmentEvidence)
    safeConvert 4  = return (Obsolete AddDocumentAttachmentEvidence)
    safeConvert 5  = return (Obsolete PendingToAwaitingAuthorEvidence)
    safeConvert 6  = return (Obsolete UpdateFieldsEvidence)
    safeConvert 7  = return (Obsolete SetElegitimationIdentificationEvidence)
    safeConvert 8  = return (Obsolete SetEmailIdentificationEvidence)
    safeConvert 9  = return (Current TimeoutDocumentEvidence)
    safeConvert 10 = return (Current SignDocumentEvidence)
    safeConvert 11 = return (Obsolete SetInvitationDeliveryStatusEvidence)
    safeConvert 12 = return (Obsolete SetDocumentUIEvidence)
    safeConvert 13 = return (Obsolete SetDocumentLangEvidence)
    safeConvert 14 = return (Obsolete SetDocumentTitleEvidence)
    safeConvert 15 = return (Obsolete SetDocumentAdvancedFunctionalityEvidence)
    safeConvert 16 = return (Obsolete RemoveDaysToSignEvidence)
    safeConvert 17 = return (Obsolete SetDaysToSignEvidence)
    safeConvert 18 = return (Obsolete SetInvitationTextEvidence)
    safeConvert 19 = return (Obsolete RemoveSignatoryUserEvidence)
    safeConvert 20 = return (Obsolete SetSignatoryUserEvidence)
    safeConvert 21 = return (Obsolete RemoveSignatoryCompanyEvidence)
    safeConvert 22 = return (Obsolete SetSignatoryCompanyEvidence)
    safeConvert 23 = return (Obsolete SetDocumentTagsEvidence)
    safeConvert 24 = return (Current SaveSigAttachmentEvidence)
    safeConvert 25 = return (Obsolete SaveDocumentForUserEvidence)
    safeConvert 26 = return (Current RestartDocumentEvidence)
    safeConvert 27 = return (Obsolete ReallyDeleteDocumentEvidence)
    safeConvert 28 = return (Obsolete NewDocumentEvidence)
    safeConvert 29 = return (Current MarkInvitationReadEvidence)
    safeConvert 30 = return (Current CloseDocumentEvidence)
    safeConvert 31 = return (Current ChangeSignatoryEmailWhenUndeliveredEvidence)
    safeConvert 32 = return (Obsolete ChangeMainfileEvidence)
    safeConvert 33 = return (Current CancelDocumenElegEvidence)
    safeConvert 34 = return (Current CancelDocumentEvidence)
    safeConvert 35 = return (Obsolete AttachFileEvidence)
    safeConvert 36 = return (Current AttachSealedFileEvidence)
    safeConvert 37 = return (Current PreparationToPendingEvidence)
    safeConvert 38 = return (Current DeleteSigAttachmentEvidence)
    safeConvert 39 = return (Obsolete AuthorUsesCSVEvidence)
    safeConvert 40 = return (Obsolete ErrorDocumentEvidence)
    safeConvert 41 = return (Obsolete MarkDocumentSeenEvidence)
    safeConvert 42 = return (Current RejectDocumentEvidence)
    safeConvert 43 = return (Obsolete SetDocumentInviteTimeEvidence)
    safeConvert 44 = return (Obsolete SetDocumentTimeoutTimeEvidence)
    safeConvert 45 = return (Obsolete RestoreArchivedDocumentEvidence)
    safeConvert 46 = return (Current InvitationEvidence)
    safeConvert 47 = return (Obsolete SignableFromDocumentIDWithUpdatedAuthorEvidence)
    safeConvert 48 = return (Obsolete ArchiveDocumentEvidence)
    safeConvert 49 = return (Obsolete ResetSignatoryDetailsEvidence)
    safeConvert 50 = return (Obsolete AdminOnlySaveForUserEvidence)
    safeConvert 51 = return (Obsolete SignableFromDocumentEvidence)
    safeConvert 52 = return (Obsolete TemplateFromDocumentEvidence)
    safeConvert 53 = return (Obsolete AttachCSVUploadEvidence)
    safeConvert 54 = return (Current SendToPadDevice)
    safeConvert 55 = return (Current RemovedFromPadDevice)
    safeConvert 56 = return (Obsolete AddSignatoryEvidence)
    safeConvert 57 = return (Obsolete RemoveSignatoryEvidence)
    safeConvert 58 = return (Obsolete AddFieldEvidence)
    safeConvert 59 = return (Obsolete RemoveFieldEvidence)
    safeConvert 60 = return (Obsolete ChangeFieldEvidence)
    safeConvert 61 = return (Current ResealedPDF)
    safeConvert 62 = return (Obsolete OldDocumentHistory)
    safeConvert 63 = return (Obsolete SetStandardAuthenticationMethodEvidence)
    safeConvert 64 = return (Obsolete SetELegAuthenticationMethodEvidence)
    safeConvert 65 = return (Obsolete SetEmailDeliveryMethodEvidence)
    safeConvert 66 = return (Obsolete SetPadDeliveryMethodEvidence)
    safeConvert 67 = return (Obsolete SetAPIDeliveryMethodEvidence)
    safeConvert 68 = return (Current ReminderSend)
    safeConvert 69 = return (Obsolete SetDocumentProcessEvidence)
    safeConvert 70 = return (Obsolete DetachFileEvidence)
    safeConvert 71 = return (Current InvitationDeliveredByEmail)
    safeConvert 72 = return (Current InvitationUndeliveredByEmail)
    safeConvert 73 = return (Current SignatoryLinkVisited)
    safeConvert 74 = return (Current ProlongDocumentEvidence)
    safeConvert 75 = return (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)
    safeConvert 76 = return (Current InvitationDeliveredBySMS)
    safeConvert 77 = return (Current InvitationUndeliveredBySMS)
    safeConvert 78 = return (Current AttachGuardtimeSealedFileEvidence)
    safeConvert 79 = return (Current AttachExtendedSealedFileEvidence)
    safeConvert 80 = return (Current ErrorSealingDocumentEvidence)
    safeConvert 81 = return (Current AutomaticReminderSent)
    safeConvert s  = Left ConvertError { convSourceValue = show s
                                       , convSourceType = "Int"
                                       , convDestType = "EvidenceEventType"
                                       , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                       }

instance Convertible EvidenceEventType SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue EvidenceEventType where
  safeConvert s = safeConvert (fromSql s :: Int)
