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
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Monoid.Space
import DB
import Doc.DocumentMonad (DocumentMonad, theDocumentID)
import qualified Control.Exception as E
import qualified HostClock.Model as HC
import IPAddress
import MinutesTime
import Data.Typeable
import User.Model
import Util.Actor
import Util.HasSomeUserInfo (getEmail, getSignatoryIdentifier)
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
    deriving (Typeable)

data InsertEvidenceEvent = InsertEvidenceEvent
                           CurrentEvidenceEventType -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           Actor                  -- Actor
    deriving (Typeable)

eventTextTemplateName :: CurrentEvidenceEventType -> String
eventTextTemplateName e =  show e ++ "Text"

signatoryLinkTemplateFields :: Monad m => SignatoryLink -> F.Fields m ()
signatoryLinkTemplateFields sl = do
  F.value "identified"  $ signatorylinkauthenticationmethod sl == ELegAuthentication
                       || not (signatoryisauthor sl || signatorylinkdeliverymethod sl == APIDelivery)
  F.value "eleg"        $ signatorylinkauthenticationmethod sl == ELegAuthentication
  F.value "sms_pin"     $ signatorylinkauthenticationmethod sl == SMSPinAuthentication
  F.value "api"         $ signatorylinkdeliverymethod sl == APIDelivery
  F.value "pad"         $ signatorylinkdeliverymethod sl == PadDelivery
  F.value "email"       $ signatorylinkdeliverymethod sl == EmailDelivery
  F.value "mobile"      $ signatorylinkdeliverymethod sl == MobileDelivery
  F.value "emailmobile" $ signatorylinkdeliverymethod sl == EmailAndMobileDelivery
  F.value "viewing"     $ not $ signatoryispartner sl
  F.value "signing"     $ signatoryispartner sl

-- | Create evidence text that goes into evidence log
evidenceLogText :: (DocumentMonad m, TemplatesMonad m, MonadDB m)
                => CurrentEvidenceEventType -> F.Fields Identity () -> Maybe SignatoryLink -> Maybe String -> Actor -> m String
evidenceLogText event textFields masl mmsg actor = do
   msignatory <-
     case masl of
       Nothing -> return Nothing
       Just sl -> Just <$> getSignatoryIdentifier sl
   let fields = do
         F.value "full" True
         F.value "actor" $ actorWho actor
         maybe (return ()) (F.value "text") mmsg
         case masl of
           Nothing -> return ()
           Just sl -> do
             F.value "signatory" $ fromMaybe "(anonymous)" msignatory
             F.value "signatory_email" $ getEmail sl
             signatoryLinkTemplateFields sl
         textFields
   ts <- getTextTemplatesByLanguage $ codeFromLang LANG_EN
   return $ runIdentity $ renderHelper ts (eventTextTemplateName event) fields

instance (DocumentMonad m, MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsg Bool where
  update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields masl mmsg actor) = do
   text <- evidenceLogText event textFields masl mmsg actor
   did <- theDocumentID
   runQuery01 . sqlInsert "evidence_log" $ do
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

instance (DocumentMonad m, MonadDB m, TemplatesMonad m) => DBUpdate m InsertEvidenceEvent Bool where
  update (InsertEvidenceEvent event textFields actor) = update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields Nothing Nothing actor)

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
  , evSigLink    :: Maybe s
  , evAPI        :: Maybe String
  , evAffectedSigLink :: Maybe s -- Some events affect only one signatory, but actor is out system or author. We express it here, since we can't with evType.
  , evMessageText :: Maybe String -- Some events have message connected to them (like reminders). We don't store such events in documents, but they should not get lost.
  }
  deriving (Eq, Ord, Show, Typeable)

data GetEvidenceLog = GetEvidenceLog DocumentID
instance MonadDB m => DBQuery m GetEvidenceLog [DocumentEvidenceEvent] where
  query (GetEvidenceLog docid) = do
    runQuery_ $ "SELECT "
      <> "  document_id"
      <> ", evidence_log.time"
      <> ", text"
      <> ", event_type"
      <> ", version_id"
      <> ", user_id"
      <> ", email"
      <> ", request_ip_v4"
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
      <> "  WHERE document_id =" <?> docid
      <> "  ORDER BY evidence_log.time, id"
    fetchMany fetchEvidenceLog
    where
      fetchEvidenceLog (did', tm, txt, tp, vid, uid, eml, ip4, slid, api, aslid, emsg, ctime, cname, hctime, offset, frequency) =
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
          , evSigLink    = slid
          , evAPI        = api
          , evAffectedSigLink = aslid
          , evMessageText = emsg
          }

copyEvidenceLogToNewDocument :: MonadDB m => DocumentID -> DocumentID -> m ()
copyEvidenceLogToNewDocument fromdoc todoc = do
  copyEvidenceLogToNewDocuments fromdoc [todoc]

copyEvidenceLogToNewDocuments :: MonadDB m => DocumentID -> [DocumentID] -> m ()
copyEvidenceLogToNewDocuments fromdoc todocs = do
  runQuery_ $ "INSERT INTO evidence_log ("
    <> "  document_id"
    <> ", time"
    <> ", text"
    <> ", event_type"
    <> ", version_id"
    <> ", user_id"
    <> ", email"
    <> ", request_ip_v4"
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

-- Evidence types that can currently be generated in the system, and
-- that will be included in the author's view and on the verification
-- page.
data CurrentEvidenceEventType =
  TimeoutDocumentEvidence                         |
  SignDocumentEvidence                            |
  SaveSigAttachmentEvidence                       |
  RestartDocumentEvidence                         |
  MarkInvitationReadEvidence                      |
  CloseDocumentEvidence                           |
  ChangeSignatoryEmailWhenUndeliveredEvidence     |
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
  AutomaticReminderSent                           |
  UpdateFieldCheckboxEvidence                     |
  UpdateFieldSignatureEvidence                    |
  UpdateFieldTextEvidence                         |
  SMSPinSendEvidence                              |
  SMSPinDeliveredEvidence
  SignWithELegFailureEvidence
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- Evidence types that are not generated anymore by the system.  Not
-- included in the author's view or on the verification page, and they
-- have no text definitions anymore.
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
  SetDocumentInviteTimeEvidence                   |
  CancelDocumenElegEvidence
  deriving (Eq, Show, Read, Ord, Enum, Bounded)


instance PQFormat EvidenceEventType where
  pqFormat _ = pqFormat (undefined::Int16)

instance ToSQL EvidenceEventType where
  type PQDest EvidenceEventType = PQDest Int16
  toSQL (Obsolete AddSigAttachmentEvidence)                        = toSQL (1::Int16)
  toSQL (Obsolete RemoveSigAttachmentsEvidence)                    = toSQL (2::Int16)
  toSQL (Obsolete RemoveDocumentAttachmentEvidence)                = toSQL (3::Int16)
  toSQL (Obsolete AddDocumentAttachmentEvidence)                   = toSQL (4::Int16)
  toSQL (Obsolete PendingToAwaitingAuthorEvidence)                 = toSQL (5::Int16)
  toSQL (Obsolete UpdateFieldsEvidence)                            = toSQL (6::Int16)
  toSQL (Obsolete SetElegitimationIdentificationEvidence)          = toSQL (7::Int16)
  toSQL (Obsolete SetEmailIdentificationEvidence)                  = toSQL (8::Int16)
  toSQL (Current TimeoutDocumentEvidence)                          = toSQL (9::Int16)
  toSQL (Current SignDocumentEvidence)                             = toSQL (10::Int16)
  toSQL (Obsolete SetInvitationDeliveryStatusEvidence)             = toSQL (11::Int16)
  toSQL (Obsolete SetDocumentUIEvidence)                           = toSQL (12::Int16)
  toSQL (Obsolete SetDocumentLangEvidence)                         = toSQL (13::Int16)
  toSQL (Obsolete SetDocumentTitleEvidence)                        = toSQL (14::Int16)
  toSQL (Obsolete SetDocumentAdvancedFunctionalityEvidence)        = toSQL (15::Int16)
  toSQL (Obsolete RemoveDaysToSignEvidence)                        = toSQL (16::Int16)
  toSQL (Obsolete SetDaysToSignEvidence)                           = toSQL (17::Int16)
  toSQL (Obsolete SetInvitationTextEvidence)                       = toSQL (18::Int16)
  toSQL (Obsolete RemoveSignatoryUserEvidence)                     = toSQL (19::Int16)
  toSQL (Obsolete SetSignatoryUserEvidence)                        = toSQL (20::Int16)
  toSQL (Obsolete RemoveSignatoryCompanyEvidence)                  = toSQL (21::Int16)
  toSQL (Obsolete SetSignatoryCompanyEvidence)                     = toSQL (22::Int16)
  toSQL (Obsolete SetDocumentTagsEvidence)                         = toSQL (23::Int16)
  toSQL (Current SaveSigAttachmentEvidence)                        = toSQL (24::Int16)
  toSQL (Obsolete SaveDocumentForUserEvidence)                     = toSQL (25::Int16)
  toSQL (Current RestartDocumentEvidence)                          = toSQL (26::Int16)
  toSQL (Obsolete ReallyDeleteDocumentEvidence)                    = toSQL (27::Int16)
  toSQL (Obsolete NewDocumentEvidence)                             = toSQL (28::Int16)
  toSQL (Current MarkInvitationReadEvidence)                       = toSQL (29::Int16)
  toSQL (Current CloseDocumentEvidence)                            = toSQL (30::Int16)
  toSQL (Current ChangeSignatoryEmailWhenUndeliveredEvidence)      = toSQL (31::Int16)
  toSQL (Obsolete ChangeMainfileEvidence)                          = toSQL (32::Int16)
  toSQL (Obsolete CancelDocumenElegEvidence)                       = toSQL (33::Int16)
  toSQL (Current CancelDocumentEvidence)                           = toSQL (34::Int16)
  toSQL (Obsolete AttachFileEvidence)                              = toSQL (35::Int16)
  toSQL (Current AttachSealedFileEvidence)                         = toSQL (36::Int16)
  toSQL (Current PreparationToPendingEvidence)                     = toSQL (37::Int16)
  toSQL (Current DeleteSigAttachmentEvidence)                      = toSQL (38::Int16)
  toSQL (Obsolete AuthorUsesCSVEvidence)                           = toSQL (39::Int16)
  toSQL (Obsolete ErrorDocumentEvidence)                           = toSQL (40::Int16)
  toSQL (Obsolete MarkDocumentSeenEvidence)                        = toSQL (41::Int16)
  toSQL (Current RejectDocumentEvidence)                           = toSQL (42::Int16)
  toSQL (Obsolete SetDocumentInviteTimeEvidence)                   = toSQL (43::Int16)
  toSQL (Obsolete SetDocumentTimeoutTimeEvidence)                  = toSQL (44::Int16)
  toSQL (Obsolete RestoreArchivedDocumentEvidence)                 = toSQL (45::Int16)
  toSQL (Current InvitationEvidence)                               = toSQL (46::Int16)
  toSQL (Obsolete SignableFromDocumentIDWithUpdatedAuthorEvidence) = toSQL (47::Int16)
  toSQL (Obsolete ArchiveDocumentEvidence)                         = toSQL (48::Int16)
  toSQL (Obsolete ResetSignatoryDetailsEvidence)                   = toSQL (49::Int16)
  toSQL (Obsolete AdminOnlySaveForUserEvidence)                    = toSQL (50::Int16)
  toSQL (Obsolete SignableFromDocumentEvidence)                    = toSQL (51::Int16)
  toSQL (Obsolete TemplateFromDocumentEvidence)                    = toSQL (52::Int16)
  toSQL (Obsolete AttachCSVUploadEvidence)                         = toSQL (53::Int16)
  toSQL (Current SendToPadDevice)                                  = toSQL (54::Int16)
  toSQL (Current RemovedFromPadDevice)                             = toSQL (55::Int16)
  toSQL (Obsolete AddSignatoryEvidence)                            = toSQL (56::Int16)
  toSQL (Obsolete RemoveSignatoryEvidence)                         = toSQL (57::Int16)
  toSQL (Obsolete AddFieldEvidence)                                = toSQL (58::Int16)
  toSQL (Obsolete RemoveFieldEvidence)                             = toSQL (59::Int16)
  toSQL (Obsolete ChangeFieldEvidence)                             = toSQL (60::Int16)
  toSQL (Current ResealedPDF)                                      = toSQL (61::Int16)
  toSQL (Obsolete OldDocumentHistory)                              = toSQL (62::Int16)
  toSQL (Obsolete SetStandardAuthenticationMethodEvidence)         = toSQL (63::Int16)
  toSQL (Obsolete SetELegAuthenticationMethodEvidence)             = toSQL (64::Int16)
  toSQL (Obsolete SetEmailDeliveryMethodEvidence)                  = toSQL (65::Int16)
  toSQL (Obsolete SetPadDeliveryMethodEvidence)                    = toSQL (66::Int16)
  toSQL (Obsolete SetAPIDeliveryMethodEvidence)                    = toSQL (67::Int16)
  toSQL (Current ReminderSend)                                     = toSQL (68::Int16)
  toSQL (Obsolete SetDocumentProcessEvidence)                      = toSQL (69::Int16)
  toSQL (Obsolete DetachFileEvidence)                              = toSQL (70::Int16)
  toSQL (Current InvitationDeliveredByEmail)                       = toSQL (71::Int16)
  toSQL (Current InvitationUndeliveredByEmail)                     = toSQL (72::Int16)
  toSQL (Current SignatoryLinkVisited)                             = toSQL (73::Int16)
  toSQL (Current ProlongDocumentEvidence)                          = toSQL (74::Int16)
  toSQL (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)      = toSQL (75::Int16)
  toSQL (Current InvitationDeliveredBySMS)                         = toSQL (76::Int16)
  toSQL (Current InvitationUndeliveredBySMS)                       = toSQL (77::Int16)
  toSQL (Current AttachGuardtimeSealedFileEvidence)                = toSQL (78::Int16)
  toSQL (Current AttachExtendedSealedFileEvidence)                 = toSQL (79::Int16)
  toSQL (Current ErrorSealingDocumentEvidence)                     = toSQL (80::Int16)
  toSQL (Current AutomaticReminderSent)                            = toSQL (81::Int16)
  toSQL (Current SignWithELegFailureEvidence)                      = toSQL (82::Int16)
  toSQL (Current UpdateFieldCheckboxEvidence)                      = toSQL (83::Int16)
  toSQL (Current UpdateFieldSignatureEvidence)                     = toSQL (84::Int16)
  toSQL (Current UpdateFieldTextEvidence)                          = toSQL (85::Int16)
  toSQL (Current SMSPinSendEvidence)                               = toSQL (86::Int16)
  toSQL (Current SMSPinDeliveredEvidence)                          = toSQL (87::Int16)

instance FromSQL EvidenceEventType where
  type PQBase EvidenceEventType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return (Obsolete AddSigAttachmentEvidence)
      2 -> return (Obsolete RemoveSigAttachmentsEvidence)
      3 -> return (Obsolete RemoveDocumentAttachmentEvidence)
      4 -> return (Obsolete AddDocumentAttachmentEvidence)
      5 -> return (Obsolete PendingToAwaitingAuthorEvidence)
      6 -> return (Obsolete UpdateFieldsEvidence)
      7 -> return (Obsolete SetElegitimationIdentificationEvidence)
      8 -> return (Obsolete SetEmailIdentificationEvidence)
      9 -> return (Current TimeoutDocumentEvidence)
      10 -> return (Current SignDocumentEvidence)
      11 -> return (Obsolete SetInvitationDeliveryStatusEvidence)
      12 -> return (Obsolete SetDocumentUIEvidence)
      13 -> return (Obsolete SetDocumentLangEvidence)
      14 -> return (Obsolete SetDocumentTitleEvidence)
      15 -> return (Obsolete SetDocumentAdvancedFunctionalityEvidence)
      16 -> return (Obsolete RemoveDaysToSignEvidence)
      17 -> return (Obsolete SetDaysToSignEvidence)
      18 -> return (Obsolete SetInvitationTextEvidence)
      19 -> return (Obsolete RemoveSignatoryUserEvidence)
      20 -> return (Obsolete SetSignatoryUserEvidence)
      21 -> return (Obsolete RemoveSignatoryCompanyEvidence)
      22 -> return (Obsolete SetSignatoryCompanyEvidence)
      23 -> return (Obsolete SetDocumentTagsEvidence)
      24 -> return (Current SaveSigAttachmentEvidence)
      25 -> return (Obsolete SaveDocumentForUserEvidence)
      26 -> return (Current RestartDocumentEvidence)
      27 -> return (Obsolete ReallyDeleteDocumentEvidence)
      28 -> return (Obsolete NewDocumentEvidence)
      29 -> return (Current MarkInvitationReadEvidence)
      30 -> return (Current CloseDocumentEvidence)
      31 -> return (Current ChangeSignatoryEmailWhenUndeliveredEvidence)
      32 -> return (Obsolete ChangeMainfileEvidence)
      33 -> return (Obsolete CancelDocumenElegEvidence)
      34 -> return (Current CancelDocumentEvidence)
      35 -> return (Obsolete AttachFileEvidence)
      36 -> return (Current AttachSealedFileEvidence)
      37 -> return (Current PreparationToPendingEvidence)
      38 -> return (Current DeleteSigAttachmentEvidence)
      39 -> return (Obsolete AuthorUsesCSVEvidence)
      40 -> return (Obsolete ErrorDocumentEvidence)
      41 -> return (Obsolete MarkDocumentSeenEvidence)
      42 -> return (Current RejectDocumentEvidence)
      43 -> return (Obsolete SetDocumentInviteTimeEvidence)
      44 -> return (Obsolete SetDocumentTimeoutTimeEvidence)
      45 -> return (Obsolete RestoreArchivedDocumentEvidence)
      46 -> return (Current InvitationEvidence)
      47 -> return (Obsolete SignableFromDocumentIDWithUpdatedAuthorEvidence)
      48 -> return (Obsolete ArchiveDocumentEvidence)
      49 -> return (Obsolete ResetSignatoryDetailsEvidence)
      50 -> return (Obsolete AdminOnlySaveForUserEvidence)
      51 -> return (Obsolete SignableFromDocumentEvidence)
      52 -> return (Obsolete TemplateFromDocumentEvidence)
      53 -> return (Obsolete AttachCSVUploadEvidence)
      54 -> return (Current SendToPadDevice)
      55 -> return (Current RemovedFromPadDevice)
      56 -> return (Obsolete AddSignatoryEvidence)
      57 -> return (Obsolete RemoveSignatoryEvidence)
      58 -> return (Obsolete AddFieldEvidence)
      59 -> return (Obsolete RemoveFieldEvidence)
      60 -> return (Obsolete ChangeFieldEvidence)
      61 -> return (Current ResealedPDF)
      62 -> return (Obsolete OldDocumentHistory)
      63 -> return (Obsolete SetStandardAuthenticationMethodEvidence)
      64 -> return (Obsolete SetELegAuthenticationMethodEvidence)
      65 -> return (Obsolete SetEmailDeliveryMethodEvidence)
      66 -> return (Obsolete SetPadDeliveryMethodEvidence)
      67 -> return (Obsolete SetAPIDeliveryMethodEvidence)
      68 -> return (Current ReminderSend)
      69 -> return (Obsolete SetDocumentProcessEvidence)
      70 -> return (Obsolete DetachFileEvidence)
      71 -> return (Current InvitationDeliveredByEmail)
      72 -> return (Current InvitationUndeliveredByEmail)
      73 -> return (Current SignatoryLinkVisited)
      74 -> return (Current ProlongDocumentEvidence)
      75 -> return (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)
      76 -> return (Current InvitationDeliveredBySMS)
      77 -> return (Current InvitationUndeliveredBySMS)
      78 -> return (Current AttachGuardtimeSealedFileEvidence)
      79 -> return (Current AttachExtendedSealedFileEvidence)
      80 -> return (Current ErrorSealingDocumentEvidence)
      81 -> return (Current AutomaticReminderSent)
      82 -> return (Current SignWithELegFailureEvidence)
      83 -> return (Current UpdateFieldCheckboxEvidence)
      84 -> return (Current UpdateFieldSignatureEvidence)
      85 -> return (Current UpdateFieldTextEvidence)
      86 -> return (Current SMSPinSendEvidence)
      87 -> return (Current SMSPinDeliveredEvidence)
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 82)]
      , reValue = n
      }
