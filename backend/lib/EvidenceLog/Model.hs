module EvidenceLog.Model (
    EvidenceEventType(..)
  , EventRenderTarget(..)
  , CurrentEvidenceEventType(..)
  , ObsoleteEvidenceEventType(..)
  , eventTextTemplateName
  , apiActor
  , InsertEvidenceEvent(..)
  , InsertEvidenceEventWithAffectedSignatoryAndMsg(..)
  , InsertEvidenceEventWithAffectedSignatoryAndMsgs(..)
  , GetEvidenceLog(..)
  , DocumentEvidenceEvent(..)
  , evidenceLogText
  , copyEvidenceLogToNewDocument
  , copyEvidenceLogToNewDocuments
  , signatoryLinkTemplateFields
  , authViewChangeToEvidence
  , authViewArchivedChangeToEvidence
  ) where

import Control.Monad.Catch
import Control.Monad.Identity
import Data.Int
import Data.List.Utils (replace)
import Data.Typeable
import Text.StringTemplates.Templates
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import DB
import DB.XML ()
import Doc.DocStateData
  ( AuthenticationToSignMethod(..), DeliveryMethod(..), SignatoryLink(..) )

import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.SignatoryLinkID
import Doc.Types.SignatoryLink
import IPAddress
import MinutesTime
import Text.XML.Content (parseXMLContent)
import Text.XML.DirtyContent (XMLContent(..))
import User.Model
import Util.Actor
import Util.HasSomeUserInfo (getEmail)
import Util.SignatoryLinkUtils
  ( getSigLinkFor, isApprover, isSignatory, isViewer )

import VersionTH
import qualified HostClock.Model as HC

data InsertEvidenceEventWithAffectedSignatoryAndMsgs = InsertEvidenceEventWithAffectedSignatoryAndMsgs
                           CurrentEvidenceEventType -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           (Maybe SignatoryLink)  -- Affected signatory
                           (Maybe String)         -- Message text
                           (Maybe String)         -- Additional message text
                           Actor                  -- Actor
    deriving (Typeable)

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

-- | Decides which template we should pick for rendering an event
data EventRenderTarget =
   EventForEvidenceLog
 | EventForArchive
  deriving (Enum, Eq, Ord, Bounded, Show)

eventTextTemplateName :: EventRenderTarget -> CurrentEvidenceEventType
                      -> String
eventTextTemplateName t e =  show e ++ suffix t
  where suffix EventForEvidenceLog       = "Log"
        suffix EventForArchive           = "Archive"

signatoryLinkTemplateFields :: Monad m => SignatoryLink -> F.Fields m ()
signatoryLinkTemplateFields sl = do
  F.value "identified"  $ signatorylinkauthenticationtosignmethod sl `elem` [SEBankIDAuthenticationToSign, NOBankIDAuthenticationToSign, DKNemIDAuthenticationToSign]
                       || not (signatoryisauthor sl || signatorylinkdeliverymethod sl == APIDelivery)
  F.value "eleg"        $ signatorylinkauthenticationtosignmethod sl `elem` [SEBankIDAuthenticationToSign, NOBankIDAuthenticationToSign, DKNemIDAuthenticationToSign]
  F.value "sms_pin"     $ signatorylinkauthenticationtosignmethod sl == SMSPinAuthenticationToSign
  F.value "api"         $ signatorylinkdeliverymethod sl == APIDelivery
  F.value "pad"         $ signatorylinkdeliverymethod sl == PadDelivery
  F.value "email"       $ signatorylinkdeliverymethod sl == EmailDelivery
  F.value "mobile"      $ signatorylinkdeliverymethod sl == MobileDelivery
  F.value "emailmobile" $ signatorylinkdeliverymethod sl == EmailAndMobileDelivery
  F.value "viewing"     $ isViewer sl
  F.value "signing"     $ isSignatory sl || signatoryrole sl == SignatoryRoleForwardedSigningParty
  F.value "approving"   $ isApprover sl || signatoryrole sl == SignatoryRoleForwardedApprover

-- | Create evidence text that goes into evidence log
evidenceLogText :: (DocumentMonad m, TemplatesMonad m, MonadDB m, MonadThrow m)
                => CurrentEvidenceEventType -> F.Fields Identity () -> Maybe SignatoryLink -> Maybe String -> Maybe String -> m XMLContent
evidenceLogText event textFields masl mmsg masg = do
   let fields = do
         F.value "full" True
       -- Interim substitutions that can be eliminated if we switch from hstringtemplates to XML for representing holes in all event texts.
         F.value "actor" ("$actor$" :: String)
         F.value "signatory" ("$signatory$" :: String)
         maybe (return ()) (F.value "text") (mmsg)
         maybe (return ()) (F.value "additional_text") (masg)
         case masl of
           Nothing -> return ()
           Just sl -> do
             F.value "signatory_email" $ getEmail sl
             signatoryLinkTemplateFields sl
         textFields
   ts <- getTextTemplatesByLanguage $ codeFromLang LANG_EN
   let n = eventTextTemplateName EventForEvidenceLog event
       -- Interim substitutions that can be eliminated if we switch from hstringtemplates to XML holes for representing holes in all event texts.
       fixIdentityVariables = replace "$actor$" "<span class='actor'/>"
                            . replace "$signatory$" "<span class='signatory'/>"
   parseEventTextTemplate n $ fixIdentityVariables $ runIdentity $ renderHelper ts n fields

parseEventTextTemplate :: MonadThrow m => String -> String -> m XMLContent
parseEventTextTemplate name s =
  either (unexpectedError . (("Cannot parse event template " ++ name ++ " with content " ++ s ++ ": ") ++) . show) (return . CleanXMLContent) $
    parseXMLContent $ T.pack s

instance (DocumentMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsgs Bool where
  update (InsertEvidenceEventWithAffectedSignatoryAndMsgs event textFields masl mmsg mamsg actor) = do
   -- FIXME: change to mmsg :: Maybe XMLContent
   text <- evidenceLogText event textFields masl mmsg mamsg
   did <- theDocumentID
   actorSLID <- theDocument >>= \doc -> return $
     actorSigLinkID actor `mplus` (signatorylinkid <$> (actorUserID actor >>= flip getSigLinkFor doc))
   runQuery01 . sqlInsert "evidence_log" $ do
      sqlSet "document_id" did
      sqlSet "time" $ actorTime actor
      sqlSet "text" text
      sqlSet "event_type" (Current event)
      sqlSet "version_id" versionID
      sqlSet "user_id" $ actorUserID actor
      sqlSet "email" $ actorEmail actor
      sqlSet "request_ip_v4" $ actorIP actor
      sqlSet "signatory_link_id" actorSLID
      sqlSet "api_user" $ actorAPIString actor
      sqlSet "affected_signatory_link_id" $ signatorylinkid <$> masl
      sqlSet "message_text" $ mmsg
      sqlSet "additional_message_text" $ mamsg
      sqlSet "client_time" $ actorClientTime actor
      sqlSet "client_name" $ actorClientName actor
      sqlSet "actor" $ actorWho actor

instance (DocumentMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => DBUpdate m InsertEvidenceEvent Bool where
  update (InsertEvidenceEvent event textFields actor) = update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields Nothing Nothing actor)

instance (DocumentMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsg Bool where
  update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields masl mmsg actor) = update (InsertEvidenceEventWithAffectedSignatoryAndMsgs event textFields masl mmsg Nothing actor)

data DocumentEvidenceEvent = DocumentEvidenceEvent {
    evDocumentID :: DocumentID
  , evTime       :: UTCTime                    -- from actor
  , evClientTime :: Maybe UTCTime              -- from actor
  , evClientName :: Maybe String               -- from actor
  , evClockErrorEstimate :: Maybe HC.ClockErrorEstimate
  , evText       :: XMLContent                 -- to go into evidence log
  , evType       :: EvidenceEventType
  , evVersionID  :: String
  , evEmail      :: Maybe String               -- from actor; use: "signatory_email" attribute if affected signatory not set
  , evUserID     :: Maybe UserID               -- from actor; use: to fetch subject name through author siglink or through account info in approximateActor; filter events
  , evIP4        :: Maybe IPAddress
  , evSigLink    :: Maybe SignatoryLinkID      -- from actor; use: to fetch subject name; viewer; filter events
  , evAPI        :: Maybe String               -- from actor; not used
  , evAffectedSigLink :: Maybe SignatoryLinkID -- Some events affect only one signatory, but actor is our system or author. We express it here, since we can't with evType.
                                               -- use: to fetch object name; viewer; to set signatoryLinkTemplateFields and "signatory" attribute; get bankID signatory name

  , evActor      :: String                     -- actorWho, used for actor identification if evSigLink is missing
  , evMessageText :: Maybe String              -- Some events have message connected to them (like reminders). We don't store such events in documents, but they should not get lost.
                                               -- use: "text" attribute
  , evAdditionalMessageText :: Maybe String    -- Some events have even more messages connected to them

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
      <> ", actor"
      <> ", message_text"
      <> ", additional_message_text"
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
      fetchEvidenceLog (did', tm, txt, tp, vid, uid, eml, ip4, slid, api, aslid, actor, emsg, eamsg, ctime, cname, hctime, offset, frequency) =
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
          , evActor      = actor
          , evMessageText = emsg
          , evAdditionalMessageText = eamsg
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
    <> ", actor"
    <> ", message_text"
    <> ", additional_message_text"
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
    <> ", actor"
    <> ", message_text"
    <> ", additional_message_text"
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
  TimeoutDocumentEvidence                          |
  SignDocumentEvidence                             |
  SaveSigAttachmentEvidence                        |
  RestartDocumentEvidence                          |
  MarkInvitationReadEvidence                       |
  CloseDocumentEvidence                            |
  CancelDocumentEvidence                           |
  AttachSealedFileEvidence                         |
  PreparationToPendingEvidence                     |
  DeleteSigAttachmentEvidence                      |
  RejectDocumentEvidence                           |
  InvitationEvidence                               |
  ResealedPDF                                      |
  ReminderSend                                     |  --Renamed
  InvitationDeliveredByEmail                       |
  InvitationUndeliveredByEmail                     |
  ProlongDocumentEvidence                          |
  ChangeSignatoryPhoneWhenUndeliveredEvidence      |
  InvitationDeliveredBySMS                         |
  InvitationUndeliveredBySMS                       |
  AttachGuardtimeSealedFileEvidence                |
  AttachExtendedSealedFileEvidence                 |
  ErrorSealingDocumentEvidence                     |
  AutomaticReminderSent                            |
  UpdateFieldCheckboxEvidence                      |
  UpdateFieldSignatureEvidence                     |
  UpdateFieldRadioGroupEvidence                    |
  SMSPinSendEvidence                               |
  SMSPinDeliveredEvidence                          |
  ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence  |
  ChangeAuthenticationToSignMethodStandardToSMSEvidence       |
  ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence  |
  ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence       |
  ChangeAuthenticationToSignMethodSMSToStandardEvidence       |
  ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence       |
  UpdateFieldFirstNameEvidence                       |
  UpdateFieldLastNameEvidence                        |
  UpdateFieldCompanyEvidence                         |
  UpdateFieldPersonalNumberEvidence                  |
  UpdateFieldCompanyNumberEvidence                   |
  UpdateFieldEmailEvidence                           |
  UpdateFieldCustomEvidence                          |
  UpdateFieldMobileEvidence                          |
  UpdateFieldNameEvidence                            |
  VisitedViewForAuthenticationEvidence               |
  VisitedViewForSigningEvidence                      |
  AuthenticatedToViewEvidence                        |
  UpdateMobileAfterIdentificationToViewWithNets      |
  ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence |
  ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence |
  ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence |
  ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence |
  ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence |
  ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence |
  ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence |
  ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence |
  ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence |
  ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence |
  ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence |
  ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence |
  AuthorAttachmentHashComputed                       |
  AuthorAttachmentAccepted                           |
  PageHighlightingAdded                              |
  PageHighlightingCleared                            |
  SignatoryAttachmentNotUploaded                     |
  ChangeSignatoryEmailEvidence                       |
  ChangeSignatoryPhoneEvidence                       |
  ChangeAuthenticationToSignMethodStandardToNOBankIDEvidence |
  ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidence |
  ChangeAuthenticationToSignMethodSMSToNOBankIDEvidence      |
  ChangeAuthenticationToSignMethodNOBankIDToStandardEvidence |
  ChangeAuthenticationToSignMethodNOBankIDToSMSEvidence      |
  ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidence |
  ConsentQuestionAnswered |
  ConsentQuestionAnsweredWithDescription |
  ChangeAuthenticationToViewMethodSMSPinToStandardEvidence  |
  ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence  |
  ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence  |
  ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence   |
  ChangeAuthenticationToViewMethodStandardToSMSPinEvidence  |
  ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence  |
  ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence  |
  ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence   |
  ChangeAuthenticationToSignMethodStandardToDKNemIDEvidence  |
  ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidence  |
  ChangeAuthenticationToSignMethodSMSToDKNemIDEvidence       |
  ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidence  |
  ChangeAuthenticationToSignMethodDKNemIDToStandardEvidence  |
  ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidence  |
  ChangeAuthenticationToSignMethodDKNemIDToSMSEvidence       |
  ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidence  |
  ChangeAuthenticationToViewMethodStandardToFITupasEvidence  |
  ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence    |
  ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence  |
  ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence  |
  ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence   |
  ChangeAuthenticationToViewMethodFITupasToStandardEvidence  |
  ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence    |
  ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence  |
  ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence  |
  ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence   |
  ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidence   |
  ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidence |
  ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidence |
  ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidence  |
  ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidence   |
  ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidence   |
  ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidence   |
  ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidence    |
  ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidence    |
  ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidence |
  ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidence   |
  ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidence |
  ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidence  |
  ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidence |
  ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidence   |
  ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidence |
  ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidence  |
  ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidence  |
  ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidence    |
  ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidence   |
  ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidence  |
  ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidence    |
  ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidence  |
  ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidence   |
  ApprovedByApproverPartyEvidence |
  RejectDocumentByApproverEvidence |
  ForwardedSigningEvidence |
  ConfirmationDeliveredByEmail |
  ConfirmationUndeliveredByEmail |
  CustomEventEvidence
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
  SetStandardAuthenticationToSignMethodEvidence   |
  SetELegAuthenticationToSignMethodEvidence       |
  AuthorUsesCSVEvidence                           |
  ErrorDocumentEvidence                           |
  SetDocumentInviteTimeEvidence                   |
  CancelDocumenElegEvidence                       |
  SendToPadDevice                                 |
  RemovedFromPadDevice                            |
  UpdateFieldTextEvidence                         |
  SignWithELegFailureEvidence                     |
  SignatoryLinkVisited                            |
  ObsoleteAuthorAttachmentAccepted                |
  ChangeSignatoryEmailWhenUndeliveredEvidence
  deriving (Eq, Show, Read, Ord, Enum, Bounded)


instance PQFormat EvidenceEventType where
  pqFormat = pqFormat @Int16

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
  toSQL (Obsolete ChangeSignatoryEmailWhenUndeliveredEvidence)     = toSQL (31::Int16)
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
  toSQL (Obsolete SendToPadDevice)                                 = toSQL (54::Int16)
  toSQL (Obsolete RemovedFromPadDevice)                            = toSQL (55::Int16)
  toSQL (Obsolete AddSignatoryEvidence)                            = toSQL (56::Int16)
  toSQL (Obsolete RemoveSignatoryEvidence)                         = toSQL (57::Int16)
  toSQL (Obsolete AddFieldEvidence)                                = toSQL (58::Int16)
  toSQL (Obsolete RemoveFieldEvidence)                             = toSQL (59::Int16)
  toSQL (Obsolete ChangeFieldEvidence)                             = toSQL (60::Int16)
  toSQL (Current ResealedPDF)                                      = toSQL (61::Int16)
  toSQL (Obsolete OldDocumentHistory)                              = toSQL (62::Int16)
  toSQL (Obsolete SetStandardAuthenticationToSignMethodEvidence)         = toSQL (63::Int16)
  toSQL (Obsolete SetELegAuthenticationToSignMethodEvidence)             = toSQL (64::Int16)
  toSQL (Obsolete SetEmailDeliveryMethodEvidence)                  = toSQL (65::Int16)
  toSQL (Obsolete SetPadDeliveryMethodEvidence)                    = toSQL (66::Int16)
  toSQL (Obsolete SetAPIDeliveryMethodEvidence)                    = toSQL (67::Int16)
  toSQL (Current ReminderSend)                                     = toSQL (68::Int16)
  toSQL (Obsolete SetDocumentProcessEvidence)                      = toSQL (69::Int16)
  toSQL (Obsolete DetachFileEvidence)                              = toSQL (70::Int16)
  toSQL (Current InvitationDeliveredByEmail)                       = toSQL (71::Int16)
  toSQL (Current InvitationUndeliveredByEmail)                     = toSQL (72::Int16)
  toSQL (Obsolete SignatoryLinkVisited)                             = toSQL (73::Int16)
  toSQL (Current ProlongDocumentEvidence)                          = toSQL (74::Int16)
  toSQL (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)      = toSQL (75::Int16)
  toSQL (Current InvitationDeliveredBySMS)                         = toSQL (76::Int16)
  toSQL (Current InvitationUndeliveredBySMS)                       = toSQL (77::Int16)
  toSQL (Current AttachGuardtimeSealedFileEvidence)                = toSQL (78::Int16)
  toSQL (Current AttachExtendedSealedFileEvidence)                 = toSQL (79::Int16)
  toSQL (Current ErrorSealingDocumentEvidence)                     = toSQL (80::Int16)
  toSQL (Current AutomaticReminderSent)                            = toSQL (81::Int16)
  toSQL (Obsolete SignWithELegFailureEvidence)                      = toSQL (82::Int16)
  toSQL (Current UpdateFieldCheckboxEvidence)                      = toSQL (83::Int16)
  toSQL (Current UpdateFieldSignatureEvidence)                     = toSQL (84::Int16)
  toSQL (Obsolete UpdateFieldTextEvidence)                         = toSQL (85::Int16)
  toSQL (Current SMSPinSendEvidence)                               = toSQL (86::Int16)
  toSQL (Current SMSPinDeliveredEvidence)                          = toSQL (87::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence) = toSQL (88::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodStandardToSMSEvidence)      = toSQL (89::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence) = toSQL (90::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence)      = toSQL (91::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSMSToStandardEvidence)      = toSQL (92::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence)      = toSQL (93::Int16)
  toSQL (Current UpdateFieldFirstNameEvidence                ) = toSQL (94::Int16)
  toSQL (Current UpdateFieldLastNameEvidence                 ) = toSQL (95::Int16)
  toSQL (Current UpdateFieldCompanyEvidence                  ) = toSQL (96::Int16)
  toSQL (Current UpdateFieldPersonalNumberEvidence           ) = toSQL (97::Int16)
  toSQL (Current UpdateFieldCompanyNumberEvidence            ) = toSQL (98::Int16)
  toSQL (Current UpdateFieldEmailEvidence                    ) = toSQL (99::Int16)
  toSQL (Current UpdateFieldCustomEvidence                   ) = toSQL (100::Int16)
  toSQL (Current UpdateFieldMobileEvidence                   ) = toSQL (101::Int16)
  toSQL (Current UpdateFieldNameEvidence                     ) = toSQL (102::Int16)
  toSQL (Current VisitedViewForAuthenticationEvidence        ) = toSQL (103::Int16)
  toSQL (Current VisitedViewForSigningEvidence               ) = toSQL (104::Int16)
  toSQL (Current AuthenticatedToViewEvidence                 ) = toSQL (105::Int16)
  toSQL (Current UpdateMobileAfterIdentificationToViewWithNets  ) = toSQL (106::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence) = toSQL (107::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence) = toSQL (108::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence) = toSQL (109::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence) = toSQL (110::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence) = toSQL (111::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence) = toSQL (112::Int16)
  toSQL (Obsolete ObsoleteAuthorAttachmentAccepted) = toSQL (113::Int16)
  toSQL (Current AuthorAttachmentHashComputed) = toSQL (114::Int16)
  toSQL (Current AuthorAttachmentAccepted) = toSQL (115::Int16)
  toSQL (Current PageHighlightingAdded) = toSQL (116::Int16)
  toSQL (Current PageHighlightingCleared) = toSQL (117::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence) = toSQL (118::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence) = toSQL (119::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence) = toSQL (120::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence) = toSQL (121::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence) = toSQL (122::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence) = toSQL (123::Int16)
  toSQL (Current SignatoryAttachmentNotUploaded) = toSQL (124::Int16)
  toSQL (Current UpdateFieldRadioGroupEvidence) = toSQL (125::Int16)
  toSQL (Current ChangeSignatoryEmailEvidence) = toSQL (126::Int16)
  toSQL (Current ChangeSignatoryPhoneEvidence) = toSQL (127::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodStandardToNOBankIDEvidence) = toSQL (128::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidence) = toSQL (129::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSMSToNOBankIDEvidence)      = toSQL (130::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodNOBankIDToStandardEvidence) = toSQL (131::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodNOBankIDToSMSEvidence)      = toSQL (132::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidence) = toSQL (133::Int16)
  toSQL (Current ConsentQuestionAnswered) = toSQL (134::Int16)
  toSQL (Current ConsentQuestionAnsweredWithDescription) = toSQL (135::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSMSPinToStandardEvidence ) = toSQL (136::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence ) = toSQL (137::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence ) = toSQL (138::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence  ) = toSQL (139::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodStandardToSMSPinEvidence ) = toSQL (140::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence ) = toSQL (141::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence ) = toSQL (142::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence  ) = toSQL (143::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodStandardToDKNemIDEvidence)  = toSQL (144::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidence)  = toSQL (145::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodSMSToDKNemIDEvidence)       = toSQL (146::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidence)  = toSQL (147::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodDKNemIDToStandardEvidence)  = toSQL (148::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidence)  = toSQL (149::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodDKNemIDToSMSEvidence)       = toSQL (150::Int16)
  toSQL (Current ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidence)  = toSQL (151::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodStandardToFITupasEvidence)  = toSQL (152::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence  )  = toSQL (153::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence)  = toSQL (154::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence)  = toSQL (155::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence )  = toSQL (156::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodFITupasToStandardEvidence)  = toSQL (157::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence  )  = toSQL (158::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence)  = toSQL (159::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence)  = toSQL (160::Int16)
  toSQL (Current ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence )  = toSQL (161::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidence) = toSQL (162::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidence) = toSQL (163::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidence) = toSQL (164::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidence) = toSQL (165::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidence) = toSQL (166::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidence) = toSQL (167::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidence) = toSQL (168::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidence) = toSQL (169::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidence) = toSQL (170::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidence) = toSQL (171::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidence) = toSQL (172::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidence) = toSQL (173::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidence) = toSQL (174::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidence) = toSQL (175::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidence) = toSQL (176::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidence) = toSQL (177::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidence) = toSQL (178::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidence) = toSQL (179::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidence) = toSQL (180::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidence) = toSQL (181::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidence) = toSQL (182::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidence) = toSQL (183::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidence) = toSQL (184::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidence) = toSQL (185::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidence) = toSQL (186::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidence) = toSQL (187::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidence) = toSQL (188::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidence) = toSQL (189::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidence) = toSQL (190::Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidence) = toSQL (191::Int16)
  toSQL (Current ApprovedByApproverPartyEvidence                                 ) = toSQL (192::Int16)
  toSQL (Current RejectDocumentByApproverEvidence                                ) = toSQL (193::Int16)
  toSQL (Current ForwardedSigningEvidence                                        ) = toSQL (194::Int16)
  toSQL (Current ConfirmationDeliveredByEmail                                    ) = toSQL (195::Int16)
  toSQL (Current ConfirmationUndeliveredByEmail                                  ) = toSQL (196::Int16)
  toSQL (Current CustomEventEvidence                                             ) = toSQL (197::Int16)

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
      31 -> return (Obsolete ChangeSignatoryEmailWhenUndeliveredEvidence)
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
      54 -> return (Obsolete SendToPadDevice)
      55 -> return (Obsolete RemovedFromPadDevice)
      56 -> return (Obsolete AddSignatoryEvidence)
      57 -> return (Obsolete RemoveSignatoryEvidence)
      58 -> return (Obsolete AddFieldEvidence)
      59 -> return (Obsolete RemoveFieldEvidence)
      60 -> return (Obsolete ChangeFieldEvidence)
      61 -> return (Current ResealedPDF)
      62 -> return (Obsolete OldDocumentHistory)
      63 -> return (Obsolete SetStandardAuthenticationToSignMethodEvidence)
      64 -> return (Obsolete SetELegAuthenticationToSignMethodEvidence)
      65 -> return (Obsolete SetEmailDeliveryMethodEvidence)
      66 -> return (Obsolete SetPadDeliveryMethodEvidence)
      67 -> return (Obsolete SetAPIDeliveryMethodEvidence)
      68 -> return (Current ReminderSend)
      69 -> return (Obsolete SetDocumentProcessEvidence)
      70 -> return (Obsolete DetachFileEvidence)
      71 -> return (Current InvitationDeliveredByEmail)
      72 -> return (Current InvitationUndeliveredByEmail)
      73 -> return (Obsolete SignatoryLinkVisited)
      74 -> return (Current ProlongDocumentEvidence)
      75 -> return (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)
      76 -> return (Current InvitationDeliveredBySMS)
      77 -> return (Current InvitationUndeliveredBySMS)
      78 -> return (Current AttachGuardtimeSealedFileEvidence)
      79 -> return (Current AttachExtendedSealedFileEvidence)
      80 -> return (Current ErrorSealingDocumentEvidence)
      81 -> return (Current AutomaticReminderSent)
      82 -> return (Obsolete SignWithELegFailureEvidence)
      83 -> return (Current UpdateFieldCheckboxEvidence)
      84 -> return (Current UpdateFieldSignatureEvidence)
      85 -> return (Obsolete UpdateFieldTextEvidence)
      86 -> return (Current SMSPinSendEvidence)
      87 -> return (Current SMSPinDeliveredEvidence)
      88 -> return (Current ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence)
      89 -> return (Current ChangeAuthenticationToSignMethodStandardToSMSEvidence )
      90 -> return (Current ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence)
      91 -> return (Current ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence     )
      92 -> return (Current ChangeAuthenticationToSignMethodSMSToStandardEvidence )
      93 -> return (Current ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence     )
      94 -> return (Current UpdateFieldFirstNameEvidence                )
      95 -> return (Current UpdateFieldLastNameEvidence                 )
      96 -> return (Current UpdateFieldCompanyEvidence                  )
      97 -> return (Current UpdateFieldPersonalNumberEvidence           )
      98 -> return (Current UpdateFieldCompanyNumberEvidence            )
      99 -> return (Current UpdateFieldEmailEvidence                    )
      100 -> return (Current UpdateFieldCustomEvidence                  )
      101 -> return (Current UpdateFieldMobileEvidence                  )
      102 -> return (Current UpdateFieldNameEvidence                    )
      103 -> return (Current VisitedViewForAuthenticationEvidence       )
      104 -> return (Current VisitedViewForSigningEvidence              )
      105 -> return (Current AuthenticatedToViewEvidence                )
      106 -> return (Current UpdateMobileAfterIdentificationToViewWithNets )
      107 -> return (Current ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence)
      108 -> return (Current ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence)
      109 -> return (Current ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence)
      110 -> return (Current ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence)
      111 -> return (Current ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence)
      112 -> return (Current ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence)
      113 -> return (Obsolete ObsoleteAuthorAttachmentAccepted)
      114 -> return (Current AuthorAttachmentHashComputed)
      115 -> return (Current AuthorAttachmentAccepted)
      116 -> return (Current PageHighlightingAdded)
      117 -> return (Current PageHighlightingCleared)
      118 -> return (Current ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence)
      119 -> return (Current ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence)
      120 -> return (Current ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence)
      121 -> return (Current ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence)
      122 -> return (Current ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence)
      123 -> return (Current ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence)
      124 -> return (Current SignatoryAttachmentNotUploaded)
      125 -> return (Current UpdateFieldRadioGroupEvidence)
      126 -> return (Current ChangeSignatoryEmailEvidence)
      127 -> return (Current ChangeSignatoryPhoneEvidence)
      128 -> return (Current ChangeAuthenticationToSignMethodStandardToNOBankIDEvidence)
      129 -> return (Current ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidence)
      130 -> return (Current ChangeAuthenticationToSignMethodSMSToNOBankIDEvidence)
      131 -> return (Current ChangeAuthenticationToSignMethodNOBankIDToStandardEvidence)
      132 -> return (Current ChangeAuthenticationToSignMethodNOBankIDToSMSEvidence)
      133 -> return (Current ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidence)
      134 -> return (Current ConsentQuestionAnswered)
      135 -> return (Current ConsentQuestionAnsweredWithDescription)
      136 -> return (Current ChangeAuthenticationToViewMethodSMSPinToStandardEvidence )
      137 -> return (Current ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence )
      138 -> return (Current ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence )
      139 -> return (Current ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence )
      140 -> return (Current ChangeAuthenticationToViewMethodStandardToSMSPinEvidence )
      141 -> return (Current ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence )
      142 -> return (Current ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence )
      143 -> return (Current ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence )
      144 -> return (Current ChangeAuthenticationToSignMethodStandardToDKNemIDEvidence)
      145 -> return (Current ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidence)
      146 -> return (Current ChangeAuthenticationToSignMethodSMSToDKNemIDEvidence)
      147 -> return (Current ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidence)
      148 -> return (Current ChangeAuthenticationToSignMethodDKNemIDToStandardEvidence)
      149 -> return (Current ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidence)
      150 -> return (Current ChangeAuthenticationToSignMethodDKNemIDToSMSEvidence)
      151 -> return (Current ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidence)
      152 -> return (Current ChangeAuthenticationToViewMethodStandardToFITupasEvidence)
      153 -> return (Current ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence  )
      154 -> return (Current ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence)
      155 -> return (Current ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence)
      156 -> return (Current ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence )
      157 -> return (Current ChangeAuthenticationToViewMethodFITupasToStandardEvidence)
      158 -> return (Current ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence  )
      159 -> return (Current ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence)
      160 -> return (Current ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence)
      161 -> return (Current ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence )
      162 -> return (Current ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidence)
      163 -> return (Current ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidence)
      164 -> return (Current ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidence)
      165 -> return (Current ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidence)
      166 -> return (Current ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidence)
      167 -> return (Current ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidence)
      168 -> return (Current ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidence)
      169 -> return (Current ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidence)
      170 -> return (Current ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidence)
      171 -> return (Current ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidence)
      172 -> return (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidence)
      173 -> return (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidence)
      174 -> return (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidence)
      175 -> return (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidence)
      176 -> return (Current ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidence)
      177 -> return (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidence)
      178 -> return (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidence)
      179 -> return (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidence)
      180 -> return (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidence)
      181 -> return (Current ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidence)
      182 -> return (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidence)
      183 -> return (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidence)
      184 -> return (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidence)
      185 -> return (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidence)
      186 -> return (Current ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidence)
      187 -> return (Current ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidence)
      188 -> return (Current ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidence)
      189 -> return (Current ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidence)
      190 -> return (Current ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidence)
      191 -> return (Current ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidence)
      192 -> return (Current ApprovedByApproverPartyEvidence                                 )
      193 -> return (Current RejectDocumentByApproverEvidence                                )
      194 -> return (Current ForwardedSigningEvidence                                        )
      195 -> return (Current ConfirmationDeliveredByEmail)
      196 -> return (Current ConfirmationUndeliveredByEmail)
      197 -> return (Current CustomEventEvidence                                             )
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 197)]
      , reValue = n
      }


authViewChangeToEvidence :: (AuthenticationToViewMethod, AuthenticationToViewMethod) -> Maybe CurrentEvidenceEventType
authViewChangeToEvidence (StandardAuthenticationToView, StandardAuthenticationToView) = Nothing
authViewChangeToEvidence (StandardAuthenticationToView, SMSPinAuthenticationToView )  = Just ChangeAuthenticationToViewMethodStandardToSMSPinEvidence
authViewChangeToEvidence (StandardAuthenticationToView, SEBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence
authViewChangeToEvidence (StandardAuthenticationToView, NOBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence
authViewChangeToEvidence (StandardAuthenticationToView, DKNemIDAuthenticationToView ) = Just ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence
authViewChangeToEvidence (StandardAuthenticationToView, FITupasAuthenticationToView ) = Just ChangeAuthenticationToViewMethodStandardToFITupasEvidence
authViewChangeToEvidence (SMSPinAuthenticationToView  , StandardAuthenticationToView) = Just ChangeAuthenticationToViewMethodSMSPinToStandardEvidence
authViewChangeToEvidence (SMSPinAuthenticationToView  , SMSPinAuthenticationToView)   = Nothing
authViewChangeToEvidence (SMSPinAuthenticationToView  , SEBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence
authViewChangeToEvidence (SMSPinAuthenticationToView  , NOBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence
authViewChangeToEvidence (SMSPinAuthenticationToView  , DKNemIDAuthenticationToView)  = Just ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence
authViewChangeToEvidence (SMSPinAuthenticationToView  , FITupasAuthenticationToView)  = Just ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence
authViewChangeToEvidence (SEBankIDAuthenticationToView, StandardAuthenticationToView) = Just ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence
authViewChangeToEvidence (SEBankIDAuthenticationToView, SMSPinAuthenticationToView)   = Just ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence
authViewChangeToEvidence (SEBankIDAuthenticationToView, SEBankIDAuthenticationToView) = Nothing
authViewChangeToEvidence (SEBankIDAuthenticationToView, NOBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence
authViewChangeToEvidence (SEBankIDAuthenticationToView, DKNemIDAuthenticationToView ) = Just ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence
authViewChangeToEvidence (SEBankIDAuthenticationToView, FITupasAuthenticationToView ) = Just ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence
authViewChangeToEvidence (NOBankIDAuthenticationToView, StandardAuthenticationToView) = Just ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence
authViewChangeToEvidence (NOBankIDAuthenticationToView, SMSPinAuthenticationToView)   = Just ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence
authViewChangeToEvidence (NOBankIDAuthenticationToView, SEBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence
authViewChangeToEvidence (NOBankIDAuthenticationToView, NOBankIDAuthenticationToView) = Nothing
authViewChangeToEvidence (NOBankIDAuthenticationToView, DKNemIDAuthenticationToView ) = Just ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence
authViewChangeToEvidence (NOBankIDAuthenticationToView, FITupasAuthenticationToView ) = Just ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence
authViewChangeToEvidence (DKNemIDAuthenticationToView , StandardAuthenticationToView) = Just ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence
authViewChangeToEvidence (DKNemIDAuthenticationToView , SMSPinAuthenticationToView)   = Just ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence
authViewChangeToEvidence (DKNemIDAuthenticationToView , SEBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence
authViewChangeToEvidence (DKNemIDAuthenticationToView , NOBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence
authViewChangeToEvidence (DKNemIDAuthenticationToView , DKNemIDAuthenticationToView ) = Nothing
authViewChangeToEvidence (DKNemIDAuthenticationToView , FITupasAuthenticationToView ) = Just ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence
authViewChangeToEvidence (FITupasAuthenticationToView , StandardAuthenticationToView) = Just ChangeAuthenticationToViewMethodFITupasToStandardEvidence
authViewChangeToEvidence (FITupasAuthenticationToView , SMSPinAuthenticationToView)   = Just ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence
authViewChangeToEvidence (FITupasAuthenticationToView , SEBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence
authViewChangeToEvidence (FITupasAuthenticationToView , NOBankIDAuthenticationToView) = Just ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence
authViewChangeToEvidence (FITupasAuthenticationToView , DKNemIDAuthenticationToView ) = Just ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence
authViewChangeToEvidence (FITupasAuthenticationToView , FITupasAuthenticationToView ) = Nothing

authViewArchivedChangeToEvidence
  :: (AuthenticationToViewMethod, AuthenticationToViewMethod)
  -> Maybe CurrentEvidenceEventType
authViewArchivedChangeToEvidence (StandardAuthenticationToView,
                                  StandardAuthenticationToView)
  = Nothing
authViewArchivedChangeToEvidence (StandardAuthenticationToView,
                                  SMSPinAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidence
authViewArchivedChangeToEvidence (StandardAuthenticationToView,
                                  SEBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidence
authViewArchivedChangeToEvidence (StandardAuthenticationToView,
                                  NOBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidence
authViewArchivedChangeToEvidence (StandardAuthenticationToView,
                                  DKNemIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidence
authViewArchivedChangeToEvidence (StandardAuthenticationToView,
                                  FITupasAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidence
authViewArchivedChangeToEvidence (SMSPinAuthenticationToView,
                                  StandardAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidence
authViewArchivedChangeToEvidence (SMSPinAuthenticationToView,
                                  SMSPinAuthenticationToView)
  = Nothing
authViewArchivedChangeToEvidence (SMSPinAuthenticationToView,
                                  SEBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidence
authViewArchivedChangeToEvidence (SMSPinAuthenticationToView,
                                  NOBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidence
authViewArchivedChangeToEvidence (SMSPinAuthenticationToView,
                                  DKNemIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidence
authViewArchivedChangeToEvidence (SMSPinAuthenticationToView,
                                  FITupasAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidence
authViewArchivedChangeToEvidence (SEBankIDAuthenticationToView,
                                  StandardAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidence
authViewArchivedChangeToEvidence (SEBankIDAuthenticationToView,
                                  SMSPinAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidence
authViewArchivedChangeToEvidence (SEBankIDAuthenticationToView,
                                  SEBankIDAuthenticationToView)
  = Nothing
authViewArchivedChangeToEvidence (SEBankIDAuthenticationToView,
                                  NOBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidence
authViewArchivedChangeToEvidence (SEBankIDAuthenticationToView,
                                  DKNemIDAuthenticationToView )
  = Just ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidence
authViewArchivedChangeToEvidence (SEBankIDAuthenticationToView,
                                  FITupasAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidence
authViewArchivedChangeToEvidence (NOBankIDAuthenticationToView,
                                  StandardAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidence
authViewArchivedChangeToEvidence (NOBankIDAuthenticationToView,
                                  SMSPinAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidence
authViewArchivedChangeToEvidence (NOBankIDAuthenticationToView,
                                  SEBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidence
authViewArchivedChangeToEvidence (NOBankIDAuthenticationToView,
                                  NOBankIDAuthenticationToView)
  = Nothing
authViewArchivedChangeToEvidence (NOBankIDAuthenticationToView,
                                  DKNemIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidence
authViewArchivedChangeToEvidence (NOBankIDAuthenticationToView,
                                  FITupasAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidence
authViewArchivedChangeToEvidence (DKNemIDAuthenticationToView,
                                  StandardAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidence
authViewArchivedChangeToEvidence (DKNemIDAuthenticationToView,
                                  SMSPinAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidence
authViewArchivedChangeToEvidence (DKNemIDAuthenticationToView,
                                  SEBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidence
authViewArchivedChangeToEvidence (DKNemIDAuthenticationToView,
                                  NOBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidence
authViewArchivedChangeToEvidence (DKNemIDAuthenticationToView,
                                  DKNemIDAuthenticationToView)
  = Nothing
authViewArchivedChangeToEvidence (DKNemIDAuthenticationToView,
                                  FITupasAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidence
authViewArchivedChangeToEvidence (FITupasAuthenticationToView,
                                  StandardAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidence
authViewArchivedChangeToEvidence (FITupasAuthenticationToView,
                                  SMSPinAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidence
authViewArchivedChangeToEvidence (FITupasAuthenticationToView,
                                  SEBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidence
authViewArchivedChangeToEvidence (FITupasAuthenticationToView,
                                  NOBankIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidence
authViewArchivedChangeToEvidence (FITupasAuthenticationToView,
                                  DKNemIDAuthenticationToView)
  = Just ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidence
authViewArchivedChangeToEvidence (FITupasAuthenticationToView,
                                  FITupasAuthenticationToView)
  = Nothing
