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
  , authToViewChangeEvidence
  , authToViewArchivedChangeEvidence
  , authToSignChangeEvidence
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
                           (Maybe Text)         -- Message text
                           (Maybe Text)         -- Additional message text
                           Actor                  -- Actor
    deriving (Typeable)

data InsertEvidenceEventWithAffectedSignatoryAndMsg = InsertEvidenceEventWithAffectedSignatoryAndMsg
                           CurrentEvidenceEventType -- A code for the event
                           (F.Fields Identity ()) -- Text for evidence
                           (Maybe SignatoryLink)  -- Affected signatory
                           (Maybe Text)         -- Message text
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

eventTextTemplateName :: EventRenderTarget -> CurrentEvidenceEventType -> Text
eventTextTemplateName t e = (showt e) <> suffix t
  where
    suffix EventForEvidenceLog = "Log"
    suffix EventForArchive     = "Archive"

signatoryLinkTemplateFields :: Monad m => SignatoryLink -> F.Fields m ()
signatoryLinkTemplateFields sl = do
  F.value "identified"
    $      signatorylinkauthenticationtosignmethod sl
    `elem` [ SEBankIDAuthenticationToSign
           , NOBankIDAuthenticationToSign
           , DKNemIDAuthenticationToSign
           ]
    ||     not (signatoryisauthor sl || signatorylinkdeliverymethod sl == APIDelivery)
  F.value "eleg"
    $      signatorylinkauthenticationtosignmethod sl
    `elem` [ SEBankIDAuthenticationToSign
           , NOBankIDAuthenticationToSign
           , DKNemIDAuthenticationToSign
           ]
  F.value "sms_pin"
    $  signatorylinkauthenticationtosignmethod sl
    == SMSPinAuthenticationToSign
  F.value "api" $ signatorylinkdeliverymethod sl == APIDelivery
  F.value "pad" $ signatorylinkdeliverymethod sl == PadDelivery
  F.value "email" $ signatorylinkdeliverymethod sl == EmailDelivery
  F.value "mobile" $ signatorylinkdeliverymethod sl == MobileDelivery
  F.value "emailmobile" $ signatorylinkdeliverymethod sl == EmailAndMobileDelivery
  F.value "portal" $ signatorylinkdeliverymethod sl == PortalDelivery
  F.value "viewing" $ isViewer sl
  F.value "signing"
    $  isSignatory sl
    || signatoryrole sl
    == SignatoryRoleForwardedSigningParty
  F.value "approving"
    $  isApprover sl
    || signatoryrole sl
    == SignatoryRoleForwardedApprover

-- | Create evidence text that goes into evidence log
evidenceLogText
  :: (DocumentMonad m, TemplatesMonad m, MonadDB m, MonadThrow m)
  => CurrentEvidenceEventType
  -> F.Fields Identity ()
  -> Maybe SignatoryLink
  -> Maybe Text
  -> Maybe Text
  -> m XMLContent
evidenceLogText event textFields masl mmsg masg = do
  let fields = do
        F.value "full" True
      -- Interim substitutions that can be eliminated if we switch from hstringtemplates to XML for representing holes in all event texts.
        F.value "actor" ("$actor$" :: String)
        F.value "signatory" ("$signatory$" :: String)
        maybe (return ()) (F.value "text")            (mmsg)
        maybe (return ()) (F.value "additional_text") (masg)
        case masl of
          Nothing -> return ()
          Just sl -> do
            F.value "signatory_email" $ getEmail sl
            signatoryLinkTemplateFields sl
        textFields
  ts <- getTextTemplatesByLanguage $ T.unpack $ codeFromLang LANG_EN
  let n                    = eventTextTemplateName EventForEvidenceLog event
      -- Interim substitutions that can be eliminated if we switch from hstringtemplates to XML holes for representing holes in all event texts.
      fixIdentityVariables = replace "$actor$" "<span class='actor'/>"
        . replace "$signatory$" "<span class='signatory'/>"
  parseEventTextTemplate n $ T.pack $ fixIdentityVariables $ runIdentity $ renderHelper
    ts
    (T.unpack n)
    fields

parseEventTextTemplate :: MonadThrow m => Text -> Text -> m XMLContent
parseEventTextTemplate name s =
  either
      ( unexpectedError
      . (("Cannot parse event template " <> name <> " with content " <> s <> ": ") <>)
      . showt
      )
      (return . CleanXMLContent)
    $ parseXMLContent
    $ s

instance (DocumentMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsgs Bool where
  -- FIXME: change to mmsg :: Maybe XMLContent
  update (InsertEvidenceEventWithAffectedSignatoryAndMsgs event textFields masl mmsg mamsg actor)
    = do
      text      <- evidenceLogText event textFields masl mmsg mamsg
      did       <- theDocumentID
      actorSLID <- theDocument >>= \doc ->
        return
          $       actorSigLinkID actor
          `mplus` (signatorylinkid <$> (actorUserID actor >>= flip getSigLinkFor doc))
      runQuery01 . sqlInsert "evidence_log" $ do
        sqlSet "document_id" did
        sqlSet "time" $ actorTime actor
        sqlSet "text"       text
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
  update (InsertEvidenceEvent event textFields actor) = update
    (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields Nothing Nothing actor
    )

instance (DocumentMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => DBUpdate m InsertEvidenceEventWithAffectedSignatoryAndMsg Bool where
  update (InsertEvidenceEventWithAffectedSignatoryAndMsg event textFields masl mmsg actor)
    = update
      (InsertEvidenceEventWithAffectedSignatoryAndMsgs event
                                                       textFields
                                                       masl
                                                       mmsg
                                                       Nothing
                                                       actor
      )

data DocumentEvidenceEvent = DocumentEvidenceEvent {
    evDocumentID :: DocumentID
  , evTime       :: UTCTime                    -- from actor
  , evClientTime :: Maybe UTCTime              -- from actor
  , evClientName :: Maybe Text               -- from actor
  , evClockErrorEstimate :: Maybe HC.ClockErrorEstimate
  , evText       :: XMLContent                 -- to go into evidence log
  , evType       :: EvidenceEventType
  , evVersionID  :: Text
  , evEmail      :: Maybe Text               -- from actor; use: "signatory_email" attribute if affected signatory not set
  , evUserID     :: Maybe UserID               -- from actor; use: to fetch subject name through author siglink or through account info in approximateActor; filter events
  , evIP4        :: Maybe IPAddress
  , evSigLink    :: Maybe SignatoryLinkID      -- from actor; use: to fetch subject name; viewer; filter events
  , evAPI        :: Maybe Text               -- from actor; not used
  , evAffectedSigLink :: Maybe SignatoryLinkID -- Some events affect only one signatory, but actor is our system or author. We express it here, since we can't with evType.
                                               -- use: to fetch object name; viewer; to set signatoryLinkTemplateFields and "signatory" attribute; get bankID signatory name

  , evActor      :: Text                     -- actorWho, used for actor identification if evSigLink is missing
  , evMessageText :: Maybe Text              -- Some events have message connected to them (like reminders). We don't store such events in documents, but they should not get lost.
                                               -- use: "text" attribute
  , evAdditionalMessageText :: Maybe Text    -- Some events have even more messages connected to them

  }
  deriving (Eq, Ord, Show, Typeable)

data GetEvidenceLog = GetEvidenceLog DocumentID
instance MonadDB m => DBQuery m GetEvidenceLog [DocumentEvidenceEvent] where
  query (GetEvidenceLog docid) = do
    runQuery_
      $   "SELECT "
      <>  "  document_id"
      <>  ", evidence_log.time"
      <>  ", text"
      <>  ", event_type"
      <>  ", version_id"
      <>  ", user_id"
      <>  ", email"
      <>  ", request_ip_v4"
      <>  ", signatory_link_id"
      <>  ", api_user"
      <>  ", affected_signatory_link_id"
      <>  ", actor"
      <>  ", message_text"
      <>  ", additional_message_text"
      <>  ", client_time"
      <>  ", client_name"
      <>  ", host_clock.time"
      <>  ", host_clock.clock_offset"
      <>  ", host_clock.clock_frequency"
      <> "  FROM evidence_log LEFT JOIN host_clock ON host_clock.time = (SELECT max(host_clock.time) FROM host_clock WHERE host_clock.time <= evidence_log.time)"
      <>  "  WHERE document_id ="
      <?> docid
      <>  "  ORDER BY evidence_log.time, id"
    fetchMany fetchEvidenceLog
    where
      fetchEvidenceLog (did', tm, txt, tp, vid, uid, eml, ip4, slid, api, aslid, actor, emsg, eamsg, ctime, cname, hctime, offset, frequency)
        = DocumentEvidenceEvent
          { evDocumentID            = did'
          , evTime                  = tm
          , evClientTime            = ctime
          , evClientName            = cname
          , evClockErrorEstimate    = HC.ClockErrorEstimate
                                      <$> hctime
                                      <*> offset
                                      <*> frequency
          , evText                  = txt
          , evType                  = tp
          , evVersionID             = vid
          , evUserID                = uid
          , evEmail                 = eml
          , evIP4                   = ip4
          , evSigLink               = slid
          , evAPI                   = api
          , evAffectedSigLink       = aslid
          , evActor                 = actor
          , evMessageText           = emsg
          , evAdditionalMessageText = eamsg
          }

copyEvidenceLogToNewDocument :: MonadDB m => DocumentID -> DocumentID -> m ()
copyEvidenceLogToNewDocument fromdoc todoc = do
  copyEvidenceLogToNewDocuments fromdoc [todoc]

copyEvidenceLogToNewDocuments :: MonadDB m => DocumentID -> [DocumentID] -> m ()
copyEvidenceLogToNewDocuments fromdoc todocs = do
  runQuery_
    $   "INSERT INTO evidence_log ("
    <>  "  document_id"
    <>  ", time"
    <>  ", text"
    <>  ", event_type"
    <>  ", version_id"
    <>  ", user_id"
    <>  ", email"
    <>  ", request_ip_v4"
    <>  ", signatory_link_id"
    <>  ", api_user"
    <>  ", affected_signatory_link_id"
    <>  ", actor"
    <>  ", message_text"
    <>  ", additional_message_text"
    <>  ", client_time"
    <>  ", client_name"
    <>  ") (SELECT "
    <>  "  todocs.id :: BIGINT"
    <>  ", time"
    <>  ", text"
    <>  ", event_type"
    <>  ", version_id"
    <>  ", user_id"
    <>  ", email"
    <>  ", request_ip_v4"
    <>  ", signatory_link_id"
    <>  ", api_user"
    <>  ", affected_signatory_link_id"
    <>  ", actor"
    <>  ", message_text"
    <>  ", additional_message_text"
    <>  ", client_time"
    <>  ", client_name"
    <>  " FROM evidence_log, (VALUES"
    <+> sqlConcatComma (map (parenthesize . sqlParam) todocs)
    <+> ") AS todocs(id)"
    <>  " WHERE evidence_log.document_id ="
    <?> fromdoc
    <+> ") ORDER BY evidence_log.id"

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
  AuthorAttachmentHashComputed                       |
  AuthorAttachmentAccepted                           |
  PageHighlightingAdded                              |
  PageHighlightingCleared                            |
  SignatoryAttachmentNotUploaded                     |
  ChangeSignatoryEmailEvidence                       |
  ChangeSignatoryPhoneEvidence                       |
  ConsentQuestionAnswered |
  ConsentQuestionAnsweredWithDescription |
  ApprovedByApproverPartyEvidence |
  RejectDocumentByApproverEvidence |
  ForwardedSigningEvidence |
  ConfirmationDeliveredByEmail |
  ConfirmationUndeliveredByEmail |
  CustomEventEvidence |
  ChangeAuthenticationToSignFromStandard |
  ChangeAuthenticationToSignFromSMSPin   |
  ChangeAuthenticationToSignFromSEBankID |
  ChangeAuthenticationToSignFromNOBankID |
  ChangeAuthenticationToSignFromDKNemID  |
  ChangeAuthenticationToSignToStandard   |
  ChangeAuthenticationToSignToSMSPin     |
  ChangeAuthenticationToSignToSEBankID   |
  ChangeAuthenticationToSignToNOBankID   |
  ChangeAuthenticationToSignToDKNemID    |
  ChangeAuthenticationToViewFromStandard |
  ChangeAuthenticationToViewFromSMSPin   |
  ChangeAuthenticationToViewFromSEBankID |
  ChangeAuthenticationToViewFromNOBankID |
  ChangeAuthenticationToViewFromDKNemID  |
  ChangeAuthenticationToViewFromFITupas  |
  ChangeAuthenticationToViewFromVerimi   |
  ChangeAuthenticationToViewFromIDIN     |
  ChangeAuthenticationToViewToStandard   |
  ChangeAuthenticationToViewToSMSPin     |
  ChangeAuthenticationToViewToSEBankID   |
  ChangeAuthenticationToViewToNOBankID   |
  ChangeAuthenticationToViewToDKNemID    |
  ChangeAuthenticationToViewToFITupas    |
  ChangeAuthenticationToViewToVerimi     |
  ChangeAuthenticationToViewToIDIN       |
  ChangeAuthenticationToViewArchivedFromStandard |
  ChangeAuthenticationToViewArchivedFromSMSPin    |
  ChangeAuthenticationToViewArchivedFromSEBankID  |
  ChangeAuthenticationToViewArchivedFromNOBankID  |
  ChangeAuthenticationToViewArchivedFromDKNemID   |
  ChangeAuthenticationToViewArchivedFromFITupas   |
  ChangeAuthenticationToViewArchivedFromVerimi    |
  ChangeAuthenticationToViewArchivedFromIDIN      |
  ChangeAuthenticationToViewArchivedToStandard    |
  ChangeAuthenticationToViewArchivedToSMSPin      |
  ChangeAuthenticationToViewArchivedToSEBankID    |
  ChangeAuthenticationToViewArchivedToNOBankID    |
  ChangeAuthenticationToViewArchivedToDKNemID     |
  ChangeAuthenticationToViewArchivedToFITupas     |
  ChangeAuthenticationToViewArchivedToVerimi      |
  ChangeAuthenticationToViewArchivedToIDIN
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
  ChangeSignatoryEmailWhenUndeliveredEvidence     |
  ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence         |
  ChangeAuthenticationToSignMethodStandardToSMSEvidence              |
  ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence         |
  ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence              |
  ChangeAuthenticationToSignMethodSMSToStandardEvidence              |
  ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence              |
  ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence         |
  ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence         |
  ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence         |
  ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence         |
  ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence         |
  ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence         |
  ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence          |
  ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence          |
  ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence          |
  ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence          |
  ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence          |
  ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence          |
  ChangeAuthenticationToSignMethodStandardToNOBankIDEvidence         |
  ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidence         |
  ChangeAuthenticationToSignMethodSMSToNOBankIDEvidence              |
  ChangeAuthenticationToSignMethodNOBankIDToStandardEvidence         |
  ChangeAuthenticationToSignMethodNOBankIDToSMSEvidence              |
  ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidence         |
  ChangeAuthenticationToViewMethodSMSPinToStandardEvidence           |
  ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence           |
  ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence           |
  ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence            |
  ChangeAuthenticationToViewMethodStandardToSMSPinEvidence           |
  ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence           |
  ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence           |
  ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence            |
  ChangeAuthenticationToSignMethodStandardToDKNemIDEvidence          |
  ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidence          |
  ChangeAuthenticationToSignMethodSMSToDKNemIDEvidence               |
  ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidence          |
  ChangeAuthenticationToSignMethodDKNemIDToStandardEvidence          |
  ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidence          |
  ChangeAuthenticationToSignMethodDKNemIDToSMSEvidence               |
  ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidence          |
  ChangeAuthenticationToViewMethodStandardToFITupasEvidence          |
  ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence            |
  ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence          |
  ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence          |
  ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence           |
  ChangeAuthenticationToViewMethodFITupasToStandardEvidence          |
  ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence            |
  ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence          |
  ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence          |
  ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence           |
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
  ChangeAuthenticationToViewMethodVerimiToSMSPinEvidence             |
  ChangeAuthenticationToViewMethodVerimiToSEBankIDEvidence           |
  ChangeAuthenticationToViewMethodVerimiToNOBankIDEvidence           |
  ChangeAuthenticationToViewMethodVerimiToDKNemIDEvidence            |
  ChangeAuthenticationToViewMethodVerimiToFITupasEvidence            |
  ChangeAuthenticationToViewMethodStandardToVerimiEvidence           |
  ChangeAuthenticationToViewMethodSEBankIDToVerimiEvidence           |
  ChangeAuthenticationToViewMethodNOBankIDToVerimeEvidence           |
  ChangeAuthenticationToViewMethodDKNemIDToVerimiEvidence            |
  ChangeAuthenticationToViewMethodFITupasToVerimiEvidence            |
  ChangeAuthenticationToViewMethodVerimiToStandardEvidence           |
  ChangeAuthenticationToViewMethodSMSPinToVerimiEvidence             |
  ChangeAuthenticationToViewArchivedMethodVerimiToSMSPinEvidence     |
  ChangeAuthenticationToViewArchivedMethodVerimiToSEBankIDEvidence   |
  ChangeAuthenticationToViewArchivedMethodVerimiToNOBankIDEvidence   |
  ChangeAuthenticationToViewArchivedMethodVerimiToDKNemIDEvidence    |
  ChangeAuthenticationToViewArchivedMethodVerimiToFITupasEvidence    |
  ChangeAuthenticationToViewArchivedMethodStandardToVerimiEvidence   |
  ChangeAuthenticationToViewArchivedMethodSEBankIDToVerimiEvidence   |
  ChangeAuthenticationToViewArchivedMethodNOBankIDToVerimeEvidence   |
  ChangeAuthenticationToViewArchivedMethodDKNemIDToVerimiEvidence    |
  ChangeAuthenticationToViewArchivedMethodFITupasToVerimiEvidence    |
  ChangeAuthenticationToViewArchivedMethodVerimiToStandardEvidence   |
  ChangeAuthenticationToViewArchivedMethodSMSPinToVerimiEvidence
  deriving (Eq, Show, Read, Ord, Enum, Bounded)


instance PQFormat EvidenceEventType where
  pqFormat = pqFormat @Int16

instance ToSQL EvidenceEventType where
  type PQDest EvidenceEventType = PQDest Int16
  toSQL (Obsolete AddSigAttachmentEvidence       ) = toSQL (1 :: Int16)
  toSQL (Obsolete RemoveSigAttachmentsEvidence   ) = toSQL (2 :: Int16)
  toSQL (Obsolete RemoveDocumentAttachmentEvidence) = toSQL (3 :: Int16)
  toSQL (Obsolete AddDocumentAttachmentEvidence  ) = toSQL (4 :: Int16)
  toSQL (Obsolete PendingToAwaitingAuthorEvidence) = toSQL (5 :: Int16)
  toSQL (Obsolete UpdateFieldsEvidence           ) = toSQL (6 :: Int16)
  toSQL (Obsolete SetElegitimationIdentificationEvidence) = toSQL (7 :: Int16)
  toSQL (Obsolete SetEmailIdentificationEvidence ) = toSQL (8 :: Int16)
  toSQL (Current  TimeoutDocumentEvidence        ) = toSQL (9 :: Int16)
  toSQL (Current  SignDocumentEvidence           ) = toSQL (10 :: Int16)
  toSQL (Obsolete SetInvitationDeliveryStatusEvidence) = toSQL (11 :: Int16)
  toSQL (Obsolete SetDocumentUIEvidence          ) = toSQL (12 :: Int16)
  toSQL (Obsolete SetDocumentLangEvidence        ) = toSQL (13 :: Int16)
  toSQL (Obsolete SetDocumentTitleEvidence       ) = toSQL (14 :: Int16)
  toSQL (Obsolete SetDocumentAdvancedFunctionalityEvidence) = toSQL (15 :: Int16)
  toSQL (Obsolete RemoveDaysToSignEvidence       ) = toSQL (16 :: Int16)
  toSQL (Obsolete SetDaysToSignEvidence          ) = toSQL (17 :: Int16)
  toSQL (Obsolete SetInvitationTextEvidence      ) = toSQL (18 :: Int16)
  toSQL (Obsolete RemoveSignatoryUserEvidence    ) = toSQL (19 :: Int16)
  toSQL (Obsolete SetSignatoryUserEvidence       ) = toSQL (20 :: Int16)
  toSQL (Obsolete RemoveSignatoryCompanyEvidence ) = toSQL (21 :: Int16)
  toSQL (Obsolete SetSignatoryCompanyEvidence    ) = toSQL (22 :: Int16)
  toSQL (Obsolete SetDocumentTagsEvidence        ) = toSQL (23 :: Int16)
  toSQL (Current  SaveSigAttachmentEvidence      ) = toSQL (24 :: Int16)
  toSQL (Obsolete SaveDocumentForUserEvidence    ) = toSQL (25 :: Int16)
  toSQL (Current  RestartDocumentEvidence        ) = toSQL (26 :: Int16)
  toSQL (Obsolete ReallyDeleteDocumentEvidence   ) = toSQL (27 :: Int16)
  toSQL (Obsolete NewDocumentEvidence            ) = toSQL (28 :: Int16)
  toSQL (Current  MarkInvitationReadEvidence     ) = toSQL (29 :: Int16)
  toSQL (Current  CloseDocumentEvidence          ) = toSQL (30 :: Int16)
  toSQL (Obsolete ChangeSignatoryEmailWhenUndeliveredEvidence) = toSQL (31 :: Int16)
  toSQL (Obsolete ChangeMainfileEvidence         ) = toSQL (32 :: Int16)
  toSQL (Obsolete CancelDocumenElegEvidence      ) = toSQL (33 :: Int16)
  toSQL (Current  CancelDocumentEvidence         ) = toSQL (34 :: Int16)
  toSQL (Obsolete AttachFileEvidence             ) = toSQL (35 :: Int16)
  toSQL (Current  AttachSealedFileEvidence       ) = toSQL (36 :: Int16)
  toSQL (Current  PreparationToPendingEvidence   ) = toSQL (37 :: Int16)
  toSQL (Current  DeleteSigAttachmentEvidence    ) = toSQL (38 :: Int16)
  toSQL (Obsolete AuthorUsesCSVEvidence          ) = toSQL (39 :: Int16)
  toSQL (Obsolete ErrorDocumentEvidence          ) = toSQL (40 :: Int16)
  toSQL (Obsolete MarkDocumentSeenEvidence       ) = toSQL (41 :: Int16)
  toSQL (Current  RejectDocumentEvidence         ) = toSQL (42 :: Int16)
  toSQL (Obsolete SetDocumentInviteTimeEvidence  ) = toSQL (43 :: Int16)
  toSQL (Obsolete SetDocumentTimeoutTimeEvidence ) = toSQL (44 :: Int16)
  toSQL (Obsolete RestoreArchivedDocumentEvidence) = toSQL (45 :: Int16)
  toSQL (Current  InvitationEvidence             ) = toSQL (46 :: Int16)
  toSQL (Obsolete SignableFromDocumentIDWithUpdatedAuthorEvidence) = toSQL (47 :: Int16)
  toSQL (Obsolete ArchiveDocumentEvidence        ) = toSQL (48 :: Int16)
  toSQL (Obsolete ResetSignatoryDetailsEvidence  ) = toSQL (49 :: Int16)
  toSQL (Obsolete AdminOnlySaveForUserEvidence   ) = toSQL (50 :: Int16)
  toSQL (Obsolete SignableFromDocumentEvidence   ) = toSQL (51 :: Int16)
  toSQL (Obsolete TemplateFromDocumentEvidence   ) = toSQL (52 :: Int16)
  toSQL (Obsolete AttachCSVUploadEvidence        ) = toSQL (53 :: Int16)
  toSQL (Obsolete SendToPadDevice                ) = toSQL (54 :: Int16)
  toSQL (Obsolete RemovedFromPadDevice           ) = toSQL (55 :: Int16)
  toSQL (Obsolete AddSignatoryEvidence           ) = toSQL (56 :: Int16)
  toSQL (Obsolete RemoveSignatoryEvidence        ) = toSQL (57 :: Int16)
  toSQL (Obsolete AddFieldEvidence               ) = toSQL (58 :: Int16)
  toSQL (Obsolete RemoveFieldEvidence            ) = toSQL (59 :: Int16)
  toSQL (Obsolete ChangeFieldEvidence            ) = toSQL (60 :: Int16)
  toSQL (Current  ResealedPDF                    ) = toSQL (61 :: Int16)
  toSQL (Obsolete OldDocumentHistory             ) = toSQL (62 :: Int16)
  toSQL (Obsolete SetStandardAuthenticationToSignMethodEvidence) = toSQL (63 :: Int16)
  toSQL (Obsolete SetELegAuthenticationToSignMethodEvidence) = toSQL (64 :: Int16)
  toSQL (Obsolete SetEmailDeliveryMethodEvidence ) = toSQL (65 :: Int16)
  toSQL (Obsolete SetPadDeliveryMethodEvidence   ) = toSQL (66 :: Int16)
  toSQL (Obsolete SetAPIDeliveryMethodEvidence   ) = toSQL (67 :: Int16)
  toSQL (Current  ReminderSend                   ) = toSQL (68 :: Int16)
  toSQL (Obsolete SetDocumentProcessEvidence     ) = toSQL (69 :: Int16)
  toSQL (Obsolete DetachFileEvidence             ) = toSQL (70 :: Int16)
  toSQL (Current  InvitationDeliveredByEmail     ) = toSQL (71 :: Int16)
  toSQL (Current  InvitationUndeliveredByEmail   ) = toSQL (72 :: Int16)
  toSQL (Obsolete SignatoryLinkVisited           ) = toSQL (73 :: Int16)
  toSQL (Current  ProlongDocumentEvidence        ) = toSQL (74 :: Int16)
  toSQL (Current ChangeSignatoryPhoneWhenUndeliveredEvidence) = toSQL (75 :: Int16)
  toSQL (Current  InvitationDeliveredBySMS       ) = toSQL (76 :: Int16)
  toSQL (Current  InvitationUndeliveredBySMS     ) = toSQL (77 :: Int16)
  toSQL (Current AttachGuardtimeSealedFileEvidence) = toSQL (78 :: Int16)
  toSQL (Current AttachExtendedSealedFileEvidence) = toSQL (79 :: Int16)
  toSQL (Current  ErrorSealingDocumentEvidence   ) = toSQL (80 :: Int16)
  toSQL (Current  AutomaticReminderSent          ) = toSQL (81 :: Int16)
  toSQL (Obsolete SignWithELegFailureEvidence    ) = toSQL (82 :: Int16)
  toSQL (Current  UpdateFieldCheckboxEvidence    ) = toSQL (83 :: Int16)
  toSQL (Current  UpdateFieldSignatureEvidence   ) = toSQL (84 :: Int16)
  toSQL (Obsolete UpdateFieldTextEvidence        ) = toSQL (85 :: Int16)
  toSQL (Current  SMSPinSendEvidence             ) = toSQL (86 :: Int16)
  toSQL (Current  SMSPinDeliveredEvidence        ) = toSQL (87 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence) =
    toSQL (88 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodStandardToSMSEvidence) =
    toSQL (89 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence) =
    toSQL (90 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence) =
    toSQL (91 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSMSToStandardEvidence) =
    toSQL (92 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence) =
    toSQL (93 :: Int16)
  toSQL (Current UpdateFieldFirstNameEvidence        ) = toSQL (94 :: Int16)
  toSQL (Current UpdateFieldLastNameEvidence         ) = toSQL (95 :: Int16)
  toSQL (Current UpdateFieldCompanyEvidence          ) = toSQL (96 :: Int16)
  toSQL (Current UpdateFieldPersonalNumberEvidence   ) = toSQL (97 :: Int16)
  toSQL (Current UpdateFieldCompanyNumberEvidence    ) = toSQL (98 :: Int16)
  toSQL (Current UpdateFieldEmailEvidence            ) = toSQL (99 :: Int16)
  toSQL (Current UpdateFieldCustomEvidence           ) = toSQL (100 :: Int16)
  toSQL (Current UpdateFieldMobileEvidence           ) = toSQL (101 :: Int16)
  toSQL (Current UpdateFieldNameEvidence             ) = toSQL (102 :: Int16)
  toSQL (Current VisitedViewForAuthenticationEvidence) = toSQL (103 :: Int16)
  toSQL (Current VisitedViewForSigningEvidence       ) = toSQL (104 :: Int16)
  toSQL (Current AuthenticatedToViewEvidence         ) = toSQL (105 :: Int16)
  toSQL (Current UpdateMobileAfterIdentificationToViewWithNets) = toSQL (106 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence) =
    toSQL (107 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence) =
    toSQL (108 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence) =
    toSQL (109 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence) =
    toSQL (110 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence) =
    toSQL (111 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence) =
    toSQL (112 :: Int16)
  toSQL (Obsolete ObsoleteAuthorAttachmentAccepted) = toSQL (113 :: Int16)
  toSQL (Current  AuthorAttachmentHashComputed    ) = toSQL (114 :: Int16)
  toSQL (Current  AuthorAttachmentAccepted        ) = toSQL (115 :: Int16)
  toSQL (Current  PageHighlightingAdded           ) = toSQL (116 :: Int16)
  toSQL (Current  PageHighlightingCleared         ) = toSQL (117 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence) =
    toSQL (118 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence) =
    toSQL (119 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence) =
    toSQL (120 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence) =
    toSQL (121 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence) =
    toSQL (122 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence) =
    toSQL (123 :: Int16)
  toSQL (Current SignatoryAttachmentNotUploaded) = toSQL (124 :: Int16)
  toSQL (Current UpdateFieldRadioGroupEvidence ) = toSQL (125 :: Int16)
  toSQL (Current ChangeSignatoryEmailEvidence  ) = toSQL (126 :: Int16)
  toSQL (Current ChangeSignatoryPhoneEvidence  ) = toSQL (127 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodStandardToNOBankIDEvidence) =
    toSQL (128 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidence) =
    toSQL (129 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSMSToNOBankIDEvidence) =
    toSQL (130 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodNOBankIDToStandardEvidence) =
    toSQL (131 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodNOBankIDToSMSEvidence) =
    toSQL (132 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidence) =
    toSQL (133 :: Int16)
  toSQL (Current ConsentQuestionAnswered               ) = toSQL (134 :: Int16)
  toSQL (Current ConsentQuestionAnsweredWithDescription) = toSQL (135 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSMSPinToStandardEvidence) =
    toSQL (136 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence) =
    toSQL (137 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence) =
    toSQL (138 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence) =
    toSQL (139 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodStandardToSMSPinEvidence) =
    toSQL (140 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence) =
    toSQL (141 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence) =
    toSQL (142 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence) =
    toSQL (143 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodStandardToDKNemIDEvidence) =
    toSQL (144 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidence) =
    toSQL (145 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodSMSToDKNemIDEvidence) =
    toSQL (146 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidence) =
    toSQL (147 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodDKNemIDToStandardEvidence) =
    toSQL (148 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidence) =
    toSQL (149 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodDKNemIDToSMSEvidence) =
    toSQL (150 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidence) =
    toSQL (151 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodStandardToFITupasEvidence) =
    toSQL (152 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence) =
    toSQL (153 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence) =
    toSQL (154 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence) =
    toSQL (155 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence) =
    toSQL (156 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodFITupasToStandardEvidence) =
    toSQL (157 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence) =
    toSQL (158 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence) =
    toSQL (159 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence) =
    toSQL (160 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence) =
    toSQL (161 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidence) =
    toSQL (162 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidence) =
    toSQL (163 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidence) =
    toSQL (164 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidence) =
    toSQL (165 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidence) =
    toSQL (166 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidence) =
    toSQL (167 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidence) =
    toSQL (168 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidence) =
    toSQL (169 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidence) =
    toSQL (170 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidence) =
    toSQL (171 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidence) =
    toSQL (172 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidence) =
    toSQL (173 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidence) =
    toSQL (174 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidence) =
    toSQL (175 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidence) =
    toSQL (176 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidence) =
    toSQL (177 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidence) =
    toSQL (178 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidence) =
    toSQL (179 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidence) =
    toSQL (180 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidence) =
    toSQL (181 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidence) =
    toSQL (182 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidence) =
    toSQL (183 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidence) =
    toSQL (184 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidence) =
    toSQL (185 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidence) =
    toSQL (186 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidence) =
    toSQL (187 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidence) =
    toSQL (188 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidence) =
    toSQL (189 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidence) =
    toSQL (190 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidence) =
    toSQL (191 :: Int16)
  toSQL (Current ApprovedByApproverPartyEvidence ) = toSQL (192 :: Int16)
  toSQL (Current RejectDocumentByApproverEvidence) = toSQL (193 :: Int16)
  toSQL (Current ForwardedSigningEvidence        ) = toSQL (194 :: Int16)
  toSQL (Current ConfirmationDeliveredByEmail    ) = toSQL (195 :: Int16)
  toSQL (Current ConfirmationUndeliveredByEmail  ) = toSQL (196 :: Int16)
  toSQL (Current CustomEventEvidence             ) = toSQL (197 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodVerimiToSMSPinEvidence) =
    toSQL (198 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodVerimiToSEBankIDEvidence) =
    toSQL (199 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodVerimiToNOBankIDEvidence) =
    toSQL (200 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodVerimiToDKNemIDEvidence) =
    toSQL (201 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodVerimiToFITupasEvidence) =
    toSQL (202 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodStandardToVerimiEvidence) =
    toSQL (203 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSEBankIDToVerimiEvidence) =
    toSQL (204 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodNOBankIDToVerimeEvidence) =
    toSQL (205 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodDKNemIDToVerimiEvidence) =
    toSQL (206 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodFITupasToVerimiEvidence) =
    toSQL (207 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodVerimiToStandardEvidence) =
    toSQL (208 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewMethodSMSPinToVerimiEvidence) =
    toSQL (209 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToSMSPinEvidence) =
    toSQL (210 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToSEBankIDEvidence) =
    toSQL (211 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToNOBankIDEvidence) =
    toSQL (212 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToDKNemIDEvidence) =
    toSQL (213 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToFITupasEvidence) =
    toSQL (214 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToVerimiEvidence) =
    toSQL (215 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToVerimiEvidence) =
    toSQL (216 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToVerimeEvidence) =
    toSQL (217 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToVerimiEvidence) =
    toSQL (218 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToVerimiEvidence) =
    toSQL (219 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToStandardEvidence) =
    toSQL (220 :: Int16)
  toSQL (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToVerimiEvidence) =
    toSQL (221 :: Int16)
  toSQL (Current ChangeAuthenticationToSignFromStandard        ) = toSQL (222 :: Int16)
  toSQL (Current ChangeAuthenticationToSignFromSMSPin          ) = toSQL (223 :: Int16)
  toSQL (Current ChangeAuthenticationToSignFromSEBankID        ) = toSQL (224 :: Int16)
  toSQL (Current ChangeAuthenticationToSignFromNOBankID        ) = toSQL (225 :: Int16)
  toSQL (Current ChangeAuthenticationToSignFromDKNemID         ) = toSQL (226 :: Int16)
  toSQL (Current ChangeAuthenticationToSignToStandard          ) = toSQL (227 :: Int16)
  toSQL (Current ChangeAuthenticationToSignToSMSPin            ) = toSQL (228 :: Int16)
  toSQL (Current ChangeAuthenticationToSignToSEBankID          ) = toSQL (229 :: Int16)
  toSQL (Current ChangeAuthenticationToSignToNOBankID          ) = toSQL (230 :: Int16)
  toSQL (Current ChangeAuthenticationToSignToDKNemID           ) = toSQL (231 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromStandard        ) = toSQL (232 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromSMSPin          ) = toSQL (233 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromSEBankID        ) = toSQL (234 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromNOBankID        ) = toSQL (235 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromDKNemID         ) = toSQL (236 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromFITupas         ) = toSQL (237 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromVerimi          ) = toSQL (238 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToStandard          ) = toSQL (239 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToSMSPin            ) = toSQL (240 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToSEBankID          ) = toSQL (241 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToNOBankID          ) = toSQL (242 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToDKNemID           ) = toSQL (243 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToFITupas           ) = toSQL (244 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToVerimi            ) = toSQL (245 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromStandard) = toSQL (246 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromSMSPin  ) = toSQL (247 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromSEBankID) = toSQL (248 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromNOBankID) = toSQL (249 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromDKNemID ) = toSQL (250 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromFITupas ) = toSQL (251 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromVerimi  ) = toSQL (252 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToStandard  ) = toSQL (253 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToSMSPin    ) = toSQL (254 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToSEBankID  ) = toSQL (255 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToNOBankID  ) = toSQL (256 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToDKNemID   ) = toSQL (257 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToFITupas   ) = toSQL (258 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToVerimi    ) = toSQL (259 :: Int16)
  toSQL (Current ChangeAuthenticationToViewFromIDIN            ) = toSQL (260 :: Int16)
  toSQL (Current ChangeAuthenticationToViewToIDIN              ) = toSQL (261 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedFromIDIN    ) = toSQL (262 :: Int16)
  toSQL (Current ChangeAuthenticationToViewArchivedToIDIN      ) = toSQL (263 :: Int16)


instance FromSQL EvidenceEventType where
  type PQBase EvidenceEventType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1   -> return (Obsolete AddSigAttachmentEvidence)
      2   -> return (Obsolete RemoveSigAttachmentsEvidence)
      3   -> return (Obsolete RemoveDocumentAttachmentEvidence)
      4   -> return (Obsolete AddDocumentAttachmentEvidence)
      5   -> return (Obsolete PendingToAwaitingAuthorEvidence)
      6   -> return (Obsolete UpdateFieldsEvidence)
      7   -> return (Obsolete SetElegitimationIdentificationEvidence)
      8   -> return (Obsolete SetEmailIdentificationEvidence)
      9   -> return (Current TimeoutDocumentEvidence)
      10  -> return (Current SignDocumentEvidence)
      11  -> return (Obsolete SetInvitationDeliveryStatusEvidence)
      12  -> return (Obsolete SetDocumentUIEvidence)
      13  -> return (Obsolete SetDocumentLangEvidence)
      14  -> return (Obsolete SetDocumentTitleEvidence)
      15  -> return (Obsolete SetDocumentAdvancedFunctionalityEvidence)
      16  -> return (Obsolete RemoveDaysToSignEvidence)
      17  -> return (Obsolete SetDaysToSignEvidence)
      18  -> return (Obsolete SetInvitationTextEvidence)
      19  -> return (Obsolete RemoveSignatoryUserEvidence)
      20  -> return (Obsolete SetSignatoryUserEvidence)
      21  -> return (Obsolete RemoveSignatoryCompanyEvidence)
      22  -> return (Obsolete SetSignatoryCompanyEvidence)
      23  -> return (Obsolete SetDocumentTagsEvidence)
      24  -> return (Current SaveSigAttachmentEvidence)
      25  -> return (Obsolete SaveDocumentForUserEvidence)
      26  -> return (Current RestartDocumentEvidence)
      27  -> return (Obsolete ReallyDeleteDocumentEvidence)
      28  -> return (Obsolete NewDocumentEvidence)
      29  -> return (Current MarkInvitationReadEvidence)
      30  -> return (Current CloseDocumentEvidence)
      31  -> return (Obsolete ChangeSignatoryEmailWhenUndeliveredEvidence)
      32  -> return (Obsolete ChangeMainfileEvidence)
      33  -> return (Obsolete CancelDocumenElegEvidence)
      34  -> return (Current CancelDocumentEvidence)
      35  -> return (Obsolete AttachFileEvidence)
      36  -> return (Current AttachSealedFileEvidence)
      37  -> return (Current PreparationToPendingEvidence)
      38  -> return (Current DeleteSigAttachmentEvidence)
      39  -> return (Obsolete AuthorUsesCSVEvidence)
      40  -> return (Obsolete ErrorDocumentEvidence)
      41  -> return (Obsolete MarkDocumentSeenEvidence)
      42  -> return (Current RejectDocumentEvidence)
      43  -> return (Obsolete SetDocumentInviteTimeEvidence)
      44  -> return (Obsolete SetDocumentTimeoutTimeEvidence)
      45  -> return (Obsolete RestoreArchivedDocumentEvidence)
      46  -> return (Current InvitationEvidence)
      47  -> return (Obsolete SignableFromDocumentIDWithUpdatedAuthorEvidence)
      48  -> return (Obsolete ArchiveDocumentEvidence)
      49  -> return (Obsolete ResetSignatoryDetailsEvidence)
      50  -> return (Obsolete AdminOnlySaveForUserEvidence)
      51  -> return (Obsolete SignableFromDocumentEvidence)
      52  -> return (Obsolete TemplateFromDocumentEvidence)
      53  -> return (Obsolete AttachCSVUploadEvidence)
      54  -> return (Obsolete SendToPadDevice)
      55  -> return (Obsolete RemovedFromPadDevice)
      56  -> return (Obsolete AddSignatoryEvidence)
      57  -> return (Obsolete RemoveSignatoryEvidence)
      58  -> return (Obsolete AddFieldEvidence)
      59  -> return (Obsolete RemoveFieldEvidence)
      60  -> return (Obsolete ChangeFieldEvidence)
      61  -> return (Current ResealedPDF)
      62  -> return (Obsolete OldDocumentHistory)
      63  -> return (Obsolete SetStandardAuthenticationToSignMethodEvidence)
      64  -> return (Obsolete SetELegAuthenticationToSignMethodEvidence)
      65  -> return (Obsolete SetEmailDeliveryMethodEvidence)
      66  -> return (Obsolete SetPadDeliveryMethodEvidence)
      67  -> return (Obsolete SetAPIDeliveryMethodEvidence)
      68  -> return (Current ReminderSend)
      69  -> return (Obsolete SetDocumentProcessEvidence)
      70  -> return (Obsolete DetachFileEvidence)
      71  -> return (Current InvitationDeliveredByEmail)
      72  -> return (Current InvitationUndeliveredByEmail)
      73  -> return (Obsolete SignatoryLinkVisited)
      74  -> return (Current ProlongDocumentEvidence)
      75  -> return (Current ChangeSignatoryPhoneWhenUndeliveredEvidence)
      76  -> return (Current InvitationDeliveredBySMS)
      77  -> return (Current InvitationUndeliveredBySMS)
      78  -> return (Current AttachGuardtimeSealedFileEvidence)
      79  -> return (Current AttachExtendedSealedFileEvidence)
      80  -> return (Current ErrorSealingDocumentEvidence)
      81  -> return (Current AutomaticReminderSent)
      82  -> return (Obsolete SignWithELegFailureEvidence)
      83  -> return (Current UpdateFieldCheckboxEvidence)
      84  -> return (Current UpdateFieldSignatureEvidence)
      85  -> return (Obsolete UpdateFieldTextEvidence)
      86  -> return (Current SMSPinSendEvidence)
      87  -> return (Current SMSPinDeliveredEvidence)
      88  -> return (Obsolete ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence)
      89  -> return (Obsolete ChangeAuthenticationToSignMethodStandardToSMSEvidence)
      90  -> return (Obsolete ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence)
      91  -> return (Obsolete ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence)
      92  -> return (Obsolete ChangeAuthenticationToSignMethodSMSToStandardEvidence)
      93  -> return (Obsolete ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence)
      94  -> return (Current UpdateFieldFirstNameEvidence)
      95  -> return (Current UpdateFieldLastNameEvidence)
      96  -> return (Current UpdateFieldCompanyEvidence)
      97  -> return (Current UpdateFieldPersonalNumberEvidence)
      98  -> return (Current UpdateFieldCompanyNumberEvidence)
      99  -> return (Current UpdateFieldEmailEvidence)
      100 -> return (Current UpdateFieldCustomEvidence)
      101 -> return (Current UpdateFieldMobileEvidence)
      102 -> return (Current UpdateFieldNameEvidence)
      103 -> return (Current VisitedViewForAuthenticationEvidence)
      104 -> return (Current VisitedViewForSigningEvidence)
      105 -> return (Current AuthenticatedToViewEvidence)
      106 -> return (Current UpdateMobileAfterIdentificationToViewWithNets)
      107 -> return (Obsolete ChangeAuthenticationToViewMethodStandardToSEBankIDEvidence)
      108 -> return (Obsolete ChangeAuthenticationToViewMethodStandardToNOBankIDEvidence)
      109 -> return (Obsolete ChangeAuthenticationToViewMethodSEBankIDToStandardEvidence)
      110 -> return (Obsolete ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidence)
      111 -> return (Obsolete ChangeAuthenticationToViewMethodNOBankIDToStandardEvidence)
      112 -> return (Obsolete ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidence)
      113 -> return (Obsolete ObsoleteAuthorAttachmentAccepted)
      114 -> return (Current AuthorAttachmentHashComputed)
      115 -> return (Current AuthorAttachmentAccepted)
      116 -> return (Current PageHighlightingAdded)
      117 -> return (Current PageHighlightingCleared)
      118 -> return (Obsolete ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidence)
      119 -> return (Obsolete ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidence)
      120 -> return (Obsolete ChangeAuthenticationToViewMethodDKNemIDToStandardEvidence)
      121 -> return (Obsolete ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidence)
      122 -> return (Obsolete ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidence)
      123 -> return (Obsolete ChangeAuthenticationToViewMethodStandardToDKNemIDEvidence)
      124 -> return (Current SignatoryAttachmentNotUploaded)
      125 -> return (Current UpdateFieldRadioGroupEvidence)
      126 -> return (Current ChangeSignatoryEmailEvidence)
      127 -> return (Current ChangeSignatoryPhoneEvidence)
      128 -> return (Obsolete ChangeAuthenticationToSignMethodStandardToNOBankIDEvidence)
      129 -> return (Obsolete ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidence)
      130 -> return (Obsolete ChangeAuthenticationToSignMethodSMSToNOBankIDEvidence)
      131 -> return (Obsolete ChangeAuthenticationToSignMethodNOBankIDToStandardEvidence)
      132 -> return (Obsolete ChangeAuthenticationToSignMethodNOBankIDToSMSEvidence)
      133 -> return (Obsolete ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidence)
      134 -> return (Current ConsentQuestionAnswered)
      135 -> return (Current ConsentQuestionAnsweredWithDescription)
      136 -> return (Obsolete ChangeAuthenticationToViewMethodSMSPinToStandardEvidence)
      137 -> return (Obsolete ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidence)
      138 -> return (Obsolete ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidence)
      139 -> return (Obsolete ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidence)
      140 -> return (Obsolete ChangeAuthenticationToViewMethodStandardToSMSPinEvidence)
      141 -> return (Obsolete ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidence)
      142 -> return (Obsolete ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidence)
      143 -> return (Obsolete ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidence)
      144 -> return (Obsolete ChangeAuthenticationToSignMethodStandardToDKNemIDEvidence)
      145 -> return (Obsolete ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidence)
      146 -> return (Obsolete ChangeAuthenticationToSignMethodSMSToDKNemIDEvidence)
      147 -> return (Obsolete ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidence)
      148 -> return (Obsolete ChangeAuthenticationToSignMethodDKNemIDToStandardEvidence)
      149 -> return (Obsolete ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidence)
      150 -> return (Obsolete ChangeAuthenticationToSignMethodDKNemIDToSMSEvidence)
      151 -> return (Obsolete ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidence)
      152 -> return (Obsolete ChangeAuthenticationToViewMethodStandardToFITupasEvidence)
      153 -> return (Obsolete ChangeAuthenticationToViewMethodSMSPinToFITupasEvidence)
      154 -> return (Obsolete ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidence)
      155 -> return (Obsolete ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidence)
      156 -> return (Obsolete ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidence)
      157 -> return (Obsolete ChangeAuthenticationToViewMethodFITupasToStandardEvidence)
      158 -> return (Obsolete ChangeAuthenticationToViewMethodFITupasToSMSPinEvidence)
      159 -> return (Obsolete ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidence)
      160 -> return (Obsolete ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidence)
      161 -> return (Obsolete ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidence)
      162 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidence)
      163 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidence)
      164 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidence)
      165 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidence)
      166 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidence)
      167 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidence)
      168 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidence)
      169 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidence)
      170 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidence)
      171 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidence)
      172 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidence)
      173 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidence)
      174 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidence)
      175 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidence)
      176 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidence)
      177 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidence)
      178 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidence)
      179 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidence)
      180 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidence)
      181 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidence)
      182 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidence)
      183 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidence)
      184 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidence)
      185 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidence)
      186 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidence)
      187 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidence)
      188 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidence)
      189 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidence)
      190 -> return
        (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidence)
      191 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidence)
      192 -> return (Current ApprovedByApproverPartyEvidence)
      193 -> return (Current RejectDocumentByApproverEvidence)
      194 -> return (Current ForwardedSigningEvidence)
      195 -> return (Current ConfirmationDeliveredByEmail)
      196 -> return (Current ConfirmationUndeliveredByEmail)
      197 -> return (Current CustomEventEvidence)
      198 -> return (Obsolete ChangeAuthenticationToViewMethodVerimiToSMSPinEvidence)
      199 -> return (Obsolete ChangeAuthenticationToViewMethodVerimiToSEBankIDEvidence)
      200 -> return (Obsolete ChangeAuthenticationToViewMethodVerimiToNOBankIDEvidence)
      201 -> return (Obsolete ChangeAuthenticationToViewMethodVerimiToDKNemIDEvidence)
      202 -> return (Obsolete ChangeAuthenticationToViewMethodVerimiToFITupasEvidence)
      203 -> return (Obsolete ChangeAuthenticationToViewMethodStandardToVerimiEvidence)
      204 -> return (Obsolete ChangeAuthenticationToViewMethodSEBankIDToVerimiEvidence)
      205 -> return (Obsolete ChangeAuthenticationToViewMethodNOBankIDToVerimeEvidence)
      206 -> return (Obsolete ChangeAuthenticationToViewMethodDKNemIDToVerimiEvidence)
      207 -> return (Obsolete ChangeAuthenticationToViewMethodFITupasToVerimiEvidence)
      208 -> return (Obsolete ChangeAuthenticationToViewMethodVerimiToStandardEvidence)
      209 -> return (Obsolete ChangeAuthenticationToViewMethodSMSPinToVerimiEvidence)
      210 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToSMSPinEvidence)
      211 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToSEBankIDEvidence)
      212 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToNOBankIDEvidence)
      213 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToDKNemIDEvidence)
      214 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToFITupasEvidence)
      215 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodStandardToVerimiEvidence)
      216 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSEBankIDToVerimiEvidence)
      217 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodNOBankIDToVerimeEvidence)
      218 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodDKNemIDToVerimiEvidence)
      219 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodFITupasToVerimiEvidence)
      220 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodVerimiToStandardEvidence)
      221 ->
        return (Obsolete ChangeAuthenticationToViewArchivedMethodSMSPinToVerimiEvidence)
      222 -> return (Current ChangeAuthenticationToSignFromStandard)
      223 -> return (Current ChangeAuthenticationToSignFromSMSPin)
      224 -> return (Current ChangeAuthenticationToSignFromSEBankID)
      225 -> return (Current ChangeAuthenticationToSignFromNOBankID)
      226 -> return (Current ChangeAuthenticationToSignFromDKNemID)
      227 -> return (Current ChangeAuthenticationToSignToStandard)
      228 -> return (Current ChangeAuthenticationToSignToSMSPin)
      229 -> return (Current ChangeAuthenticationToSignToSEBankID)
      230 -> return (Current ChangeAuthenticationToSignToNOBankID)
      231 -> return (Current ChangeAuthenticationToSignToDKNemID)
      232 -> return (Current ChangeAuthenticationToViewFromStandard)
      233 -> return (Current ChangeAuthenticationToViewFromSMSPin)
      234 -> return (Current ChangeAuthenticationToViewFromSEBankID)
      235 -> return (Current ChangeAuthenticationToViewFromNOBankID)
      236 -> return (Current ChangeAuthenticationToViewFromDKNemID)
      237 -> return (Current ChangeAuthenticationToViewFromFITupas)
      238 -> return (Current ChangeAuthenticationToViewFromVerimi)
      239 -> return (Current ChangeAuthenticationToViewToStandard)
      240 -> return (Current ChangeAuthenticationToViewToSMSPin)
      241 -> return (Current ChangeAuthenticationToViewToSEBankID)
      242 -> return (Current ChangeAuthenticationToViewToNOBankID)
      243 -> return (Current ChangeAuthenticationToViewToDKNemID)
      244 -> return (Current ChangeAuthenticationToViewToFITupas)
      245 -> return (Current ChangeAuthenticationToViewToVerimi)
      246 -> return (Current ChangeAuthenticationToViewArchivedFromStandard)
      247 -> return (Current ChangeAuthenticationToViewArchivedFromSMSPin)
      248 -> return (Current ChangeAuthenticationToViewArchivedFromSEBankID)
      249 -> return (Current ChangeAuthenticationToViewArchivedFromNOBankID)
      250 -> return (Current ChangeAuthenticationToViewArchivedFromDKNemID)
      251 -> return (Current ChangeAuthenticationToViewArchivedFromFITupas)
      252 -> return (Current ChangeAuthenticationToViewArchivedFromVerimi)
      253 -> return (Current ChangeAuthenticationToViewArchivedToStandard)
      254 -> return (Current ChangeAuthenticationToViewArchivedToSMSPin)
      255 -> return (Current ChangeAuthenticationToViewArchivedToSEBankID)
      256 -> return (Current ChangeAuthenticationToViewArchivedToNOBankID)
      257 -> return (Current ChangeAuthenticationToViewArchivedToDKNemID)
      258 -> return (Current ChangeAuthenticationToViewArchivedToFITupas)
      259 -> return (Current ChangeAuthenticationToViewArchivedToVerimi)
      260 -> return (Current ChangeAuthenticationToViewFromIDIN)
      261 -> return (Current ChangeAuthenticationToViewToIDIN)
      262 -> return (Current ChangeAuthenticationToViewArchivedFromIDIN)
      263 -> return (Current ChangeAuthenticationToViewArchivedToIDIN)
      _   -> E.throwIO $ RangeError { reRange = [(1, 263)], reValue = n }


authToViewChangeEvidence
  :: AuthenticationToViewMethod
  -> AuthenticationToViewMethod
  -> [CurrentEvidenceEventType]
authToViewChangeEvidence aFrom aTo =
  if (aFrom == aTo) then [] else [authToViewChangeFrom aFrom, authToViewChangeTo aTo]

authToViewChangeFrom :: AuthenticationToViewMethod -> CurrentEvidenceEventType
authToViewChangeFrom a = case a of
  StandardAuthenticationToView -> ChangeAuthenticationToViewFromStandard
  SEBankIDAuthenticationToView -> ChangeAuthenticationToViewFromSEBankID
  NOBankIDAuthenticationToView -> ChangeAuthenticationToViewFromNOBankID
  DKNemIDAuthenticationToView  -> ChangeAuthenticationToViewFromDKNemID
  SMSPinAuthenticationToView   -> ChangeAuthenticationToViewFromSMSPin
  FITupasAuthenticationToView  -> ChangeAuthenticationToViewFromFITupas
  VerimiAuthenticationToView   -> ChangeAuthenticationToViewFromVerimi
  IDINAuthenticationToView     -> ChangeAuthenticationToViewFromIDIN

authToViewChangeTo :: AuthenticationToViewMethod -> CurrentEvidenceEventType
authToViewChangeTo a = case a of
  StandardAuthenticationToView -> ChangeAuthenticationToViewToStandard
  SEBankIDAuthenticationToView -> ChangeAuthenticationToViewToSEBankID
  NOBankIDAuthenticationToView -> ChangeAuthenticationToViewToNOBankID
  DKNemIDAuthenticationToView  -> ChangeAuthenticationToViewToDKNemID
  SMSPinAuthenticationToView   -> ChangeAuthenticationToViewToSMSPin
  FITupasAuthenticationToView  -> ChangeAuthenticationToViewToFITupas
  VerimiAuthenticationToView   -> ChangeAuthenticationToViewToVerimi
  IDINAuthenticationToView     -> ChangeAuthenticationToViewToIDIN

authToViewArchivedChangeEvidence
  :: AuthenticationToViewMethod
  -> AuthenticationToViewMethod
  -> [CurrentEvidenceEventType]
authToViewArchivedChangeEvidence aFrom aTo = if (aFrom == aTo)
  then []
  else [authToViewArchivedChangeFrom aFrom, authToViewArchivedChangeTo aTo]

authToViewArchivedChangeFrom :: AuthenticationToViewMethod -> CurrentEvidenceEventType
authToViewArchivedChangeFrom a = case a of
  StandardAuthenticationToView -> ChangeAuthenticationToViewArchivedFromStandard
  SEBankIDAuthenticationToView -> ChangeAuthenticationToViewArchivedFromSEBankID
  NOBankIDAuthenticationToView -> ChangeAuthenticationToViewArchivedFromNOBankID
  DKNemIDAuthenticationToView  -> ChangeAuthenticationToViewArchivedFromDKNemID
  SMSPinAuthenticationToView   -> ChangeAuthenticationToViewArchivedFromSMSPin
  FITupasAuthenticationToView  -> ChangeAuthenticationToViewArchivedFromFITupas
  VerimiAuthenticationToView   -> ChangeAuthenticationToViewArchivedFromVerimi
  IDINAuthenticationToView     -> ChangeAuthenticationToViewArchivedFromIDIN

authToViewArchivedChangeTo :: AuthenticationToViewMethod -> CurrentEvidenceEventType
authToViewArchivedChangeTo a = case a of
  StandardAuthenticationToView -> ChangeAuthenticationToViewArchivedToStandard
  SEBankIDAuthenticationToView -> ChangeAuthenticationToViewArchivedToSEBankID
  NOBankIDAuthenticationToView -> ChangeAuthenticationToViewArchivedToNOBankID
  DKNemIDAuthenticationToView  -> ChangeAuthenticationToViewArchivedToDKNemID
  SMSPinAuthenticationToView   -> ChangeAuthenticationToViewArchivedToSMSPin
  FITupasAuthenticationToView  -> ChangeAuthenticationToViewArchivedToFITupas
  VerimiAuthenticationToView   -> ChangeAuthenticationToViewArchivedToVerimi
  IDINAuthenticationToView     -> ChangeAuthenticationToViewArchivedFromIDIN

authToSignChangeEvidence
  :: AuthenticationToSignMethod
  -> AuthenticationToSignMethod
  -> [CurrentEvidenceEventType]
authToSignChangeEvidence aFrom aTo =
  if (aFrom == aTo) then [] else [authToSignChangeFrom aFrom, authToSignChangeTo aTo]

authToSignChangeFrom :: AuthenticationToSignMethod -> CurrentEvidenceEventType
authToSignChangeFrom a = case a of
  StandardAuthenticationToSign -> ChangeAuthenticationToSignFromStandard
  SEBankIDAuthenticationToSign -> ChangeAuthenticationToSignFromSEBankID
  NOBankIDAuthenticationToSign -> ChangeAuthenticationToSignFromNOBankID
  DKNemIDAuthenticationToSign  -> ChangeAuthenticationToSignFromDKNemID
  SMSPinAuthenticationToSign   -> ChangeAuthenticationToSignFromSMSPin

authToSignChangeTo :: AuthenticationToSignMethod -> CurrentEvidenceEventType
authToSignChangeTo a = case a of
  StandardAuthenticationToSign -> ChangeAuthenticationToSignToStandard
  SEBankIDAuthenticationToSign -> ChangeAuthenticationToSignToSEBankID
  NOBankIDAuthenticationToSign -> ChangeAuthenticationToSignToNOBankID
  DKNemIDAuthenticationToSign  -> ChangeAuthenticationToSignToDKNemID
  SMSPinAuthenticationToSign   -> ChangeAuthenticationToSignToSMSPin
