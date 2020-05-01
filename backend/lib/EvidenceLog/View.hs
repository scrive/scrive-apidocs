module EvidenceLog.View (
      eventsJSListFromEvidenceLog
    , getSignatoryIdentifierMap
    , approximateActor
    , suppressRepeatedEvents
    , historyEventType
    , simplifiedEventText
  ) where

import Control.Monad.Catch
import Text.JSON
import Text.JSON.Gen as J
import Text.StringTemplates.Templates
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import DB
import Doc.DocStateData
import Doc.Model (GetDocumentsBySignatoryLinkIDs(..))
import Doc.SignatoryIdentification
  ( SignatoryIdentifierMap, siFullName, siLink, signatoryIdentifier
  , signatoryIdentifierMap
  )
import EID.Authentication.Model
import EID.EIDService.Types
import EID.Nets.Types
import EID.Signature.Model
import EvidenceLog.Model
import MinutesTime
import Templates (renderTextTemplate, renderTextTemplate_)
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog
  :: (MonadDB m, MonadThrow m, TemplatesMonad m)
  => Document
  -> [DocumentEvidenceEvent]
  -> m [JSValue]
eventsJSListFromEvidenceLog doc dees = do
  let evs = filter ((historyEventType . evType) && (not . emptyEvent))
        $ cleanUnimportantAfterSigning dees
  sim <- getSignatoryIdentifierMap True evs
  mapM (J.runJSONGenT . eventJSValue doc sim) evs

-- | Get signatory identifier map from event list
getSignatoryIdentifierMap
  :: (MonadDB m, MonadThrow m)
  => Bool
  -> [DocumentEvidenceEvent]
  -> m SignatoryIdentifierMap
getSignatoryIdentifierMap includeviewers evs = do
  let sigs = Set.fromList . catMaybes $ concat
        [ [evSigLink ev, evAffectedSigLink ev] | ev <- evs ]
  docs <- dbQuery . GetDocumentsBySignatoryLinkIDs $ Set.toList sigs
  return $ signatoryIdentifierMap includeviewers docs sigs

-- TODO: Consider saving actor name in event instead, this is likely
-- to become broken
approximateActor
  :: forall m
   . (MonadDB m, MonadThrow m, TemplatesMonad m)
  => Document
  -> SignatoryIdentifierMap
  -> DocumentEvidenceEvent
  -> m Text
approximateActor doc sim dee
  | systemEvents $ evType dee = return "Scrive"
  | otherwise = do
    emptyNamePlaceholder <- renderTextTemplate_ "_notNamedParty"
    case evSigLink dee >>= sigid emptyNamePlaceholder of
      Just i  -> return i
      Nothing -> case evUserID dee of
        Just uid -> if isAuthor (doc, uid)
          then authorName emptyNamePlaceholder
          else do
            muser <- dbQuery $ GetUserByID uid
            case muser of
              Just user -> return $ getSmartName user <> " (" <> getEmail user <> ")"
              Nothing   -> return "Scrive" -- This should not happen
        Nothing -> if authorEvents $ evType dee
          then authorName emptyNamePlaceholder
          else return "Scrive"
  where
    authorName :: Text -> m Text
    authorName emptyNamePlaceholder =
      case getAuthorSigLink doc >>= sigid emptyNamePlaceholder . signatorylinkid of
        Just i  -> return i
        Nothing -> renderTextTemplate_ "_authorParty"

    sigid emptyNamePlaceholder s = do
      si <- Map.lookup s sim
      let name = siFullName si
      if T.null name then signatoryIdentifier sim s emptyNamePlaceholder else return name

eventJSValue
  :: (MonadDB m, MonadThrow m, TemplatesMonad m)
  => Document
  -> SignatoryIdentifierMap
  -> DocumentEvidenceEvent
  -> JSONGenT m ()
eventJSValue doc sim dee = do
  J.value "status" . show $ getEvidenceEventStatusClass (evType dee)
  J.value "time" $ formatTimeISO (evTime dee)
  J.valueM "party" $ approximateActor doc sim dee
  J.valueM "text" $ simplifiedEventText Nothing sim dee


-- | Events to be included in archive history. They have translations.
historyEventType :: EvidenceEventType -> Bool
historyEventType (Current AttachExtendedSealedFileEvidence) = True
historyEventType (Current AttachDigitalSignatureSealedFileEvidence) = True
historyEventType (Obsolete CancelDocumenElegEvidence) = True
historyEventType (Current CancelDocumentEvidence) = True
historyEventType (Current InvitationDeliveredByEmail) = True
historyEventType (Current InvitationDeliveredBySMS) = True
historyEventType (Current InvitationEvidence) = True
historyEventType (Current InvitationUndeliveredByEmail) = True
historyEventType (Current InvitationUndeliveredBySMS) = True
historyEventType (Current MarkInvitationReadEvidence) = True
historyEventType (Current PreparationToPendingEvidence) = True
historyEventType (Current ProlongDocumentEvidence) = True
historyEventType (Current RejectDocumentEvidence) = True
historyEventType (Current RejectDocumentByApproverEvidence) = True
historyEventType (Current ReminderSend) = True
historyEventType (Current AutomaticReminderSent) = True
historyEventType (Current RestartDocumentEvidence) = True
historyEventType (Current SignDocumentEvidence) = True
historyEventType (Obsolete SignatoryLinkVisited) = True
historyEventType (Current TimeoutDocumentEvidence) = True
historyEventType (Current SMSPinSendEvidence) = True
historyEventType (Current SMSPinDeliveredEvidence) = True
historyEventType (Current VisitedViewForAuthenticationEvidence) = True
historyEventType (Current VisitedViewForSigningEvidence) = True
historyEventType (Current AuthenticatedToViewEvidence) = True
historyEventType (Current ApprovedByApproverPartyEvidence) = True
historyEventType (Current ForwardedSigningEvidence) = True
historyEventType _ = False


getEvidenceEventStatusClass :: EvidenceEventType -> StatusClass
getEvidenceEventStatusClass (Current  CloseDocumentEvidence       ) = SCSigned
getEvidenceEventStatusClass (Current  CancelDocumentEvidence      ) = SCCancelled
getEvidenceEventStatusClass (Current  RejectDocumentEvidence      ) = SCRejected
getEvidenceEventStatusClass (Current RejectDocumentByApproverEvidence) = SCRejected
getEvidenceEventStatusClass (Current  TimeoutDocumentEvidence     ) = SCTimedout
getEvidenceEventStatusClass (Current  PreparationToPendingEvidence) = SCInitiated
getEvidenceEventStatusClass (Current  MarkInvitationReadEvidence  ) = SCRead
getEvidenceEventStatusClass (Obsolete SignatoryLinkVisited        ) = SCOpened
getEvidenceEventStatusClass (Current  RestartDocumentEvidence     ) = SCDraft
getEvidenceEventStatusClass (Current  SignDocumentEvidence        ) = SCSigned
getEvidenceEventStatusClass (Current  InvitationEvidence          ) = SCSent
getEvidenceEventStatusClass (Current  InvitationDeliveredByEmail  ) = SCDelivered
getEvidenceEventStatusClass (Current  InvitationUndeliveredByEmail) = SCDeliveryProblem
getEvidenceEventStatusClass (Current  InvitationDeliveredBySMS    ) = SCDelivered
getEvidenceEventStatusClass (Current  InvitationUndeliveredBySMS  ) = SCDeliveryProblem
getEvidenceEventStatusClass (Current  ReminderSend                ) = SCSent
getEvidenceEventStatusClass (Current  AutomaticReminderSent       ) = SCSent
getEvidenceEventStatusClass (Current  ResealedPDF                 ) = SCSigned
getEvidenceEventStatusClass (Obsolete CancelDocumenElegEvidence   ) = SCCancelled
getEvidenceEventStatusClass (Current  ProlongDocumentEvidence     ) = SCProlonged
getEvidenceEventStatusClass (Current  AttachSealedFileEvidence    ) = SCSigned
getEvidenceEventStatusClass (Current AttachDigitalSignatureSealedFileEvidence) = SCSealed
getEvidenceEventStatusClass (Current AttachExtendedSealedFileEvidence) = SCExtended
getEvidenceEventStatusClass (Current  SMSPinSendEvidence          ) = SCSent
getEvidenceEventStatusClass (Current  SMSPinDeliveredEvidence     ) = SCDelivered
getEvidenceEventStatusClass (Current VisitedViewForAuthenticationEvidence) =
  SCOpenedAuthToView
getEvidenceEventStatusClass (Current VisitedViewForSigningEvidence) = SCOpened
getEvidenceEventStatusClass (Current AuthenticatedToViewEvidence) = SCAuthenticatedToView
getEvidenceEventStatusClass (Current ApprovedByApproverPartyEvidence) = SCApproved
getEvidenceEventStatusClass (Current ConfirmationDeliveredByEmail) = SCDelivered
getEvidenceEventStatusClass (Current ConfirmationUndeliveredByEmail) = SCDeliveryProblem
getEvidenceEventStatusClass (Current ForwardedSigningEvidence) = SCSent
getEvidenceEventStatusClass _ = SCError

-- Remove signatory events that happen after signing (link visited,
-- invitation read).
cleanUnimportantAfterSigning :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
cleanUnimportantAfterSigning = go Set.empty
  where
    go _ [] = []
    go m (e : es)
      | evType e
        `elem`       [Obsolete SignatoryLinkVisited, Current MarkInvitationReadEvidence]
        &&           ids e
        `Set.member` m
      = go m es
      | -- The only place for skipping events, but these
                -- events always have evSigLink == Just ...
        evType e
        == Current SignDocumentEvidence
        || evType e
        == Current ApprovedByApproverPartyEvidence
      = e : go (Set.insert (ids e) m) es
      | evType e == Current PreparationToPendingEvidence
      = e : go Set.empty es
      | otherwise
      = e : go m es

    ids e = (evUserID e, evSigLink e)

-- Events that should be considered as performed as author even is
-- actor states different.
authorEvents :: EvidenceEventType -> Bool
authorEvents (Current PreparationToPendingEvidence) = True
authorEvents _ = False

-- Events that should be considered as performed by the system even if
-- actor states different.
systemEvents :: EvidenceEventType -> Bool
systemEvents (Current InvitationDeliveredByEmail) = True
systemEvents (Current InvitationUndeliveredByEmail) = True
systemEvents (Current InvitationDeliveredBySMS) = True
systemEvents (Current InvitationUndeliveredBySMS) = True
systemEvents (Current ConfirmationDeliveredByEmail) = True
systemEvents (Current ConfirmationUndeliveredByEmail) = True
systemEvents _ = False

-- Empty events - they should be skipped, as they don't provide enough
-- information to show to user.
emptyEvent :: DocumentEvidenceEvent -> Bool
emptyEvent DocumentEvidenceEvent { evType = Current InvitationEvidence, evAffectedSigLink = Nothing }
  = True
emptyEvent DocumentEvidenceEvent { evType = Current ReminderSend, evAffectedSigLink = Nothing }
  = True
emptyEvent _ = False

-- | Produce simplified text for an event (only for archive or
-- verification pages).
simplifiedEventText
  :: forall m
   . (MonadDB m, MonadThrow m, TemplatesMonad m)
  => Maybe Text
  -> SignatoryIdentifierMap
  -> DocumentEvidenceEvent
  -> m Text
simplifiedEventText mactor sim dee = do
  emptyNamePlaceholder <- renderTextTemplate_ "_notNamedParty"
  case evType dee of
    Obsolete CancelDocumenElegEvidence ->
      renderEvent emptyNamePlaceholder "CancelDocumenElegEvidenceText"
    Obsolete SignatoryLinkVisited ->
      renderEvent emptyNamePlaceholder "SignatoryLinkVisitedArchive"
    Current et ->
      renderEvent emptyNamePlaceholder $ eventTextTemplateName EventForArchive et
    Obsolete _ -> return "" -- shouldn't we throw an error in this case?
  where
    renderEvent :: Text -> Text -> m Text
    renderEvent emptyNamePlaceholder eventTemplateName =
      renderTextTemplate eventTemplateName $ do
        let mslinkid = evAffectedSigLink dee
        F.forM_ mslinkid $ \slinkid -> do
          case Map.lookup slinkid sim >>= siLink of
            Just slink -> do
              signatoryLinkTemplateFields slink
              -- FIXME: fetching email from signatory is not guaranteed to get
              -- the email address field of the signatory at the time of the
              -- event, since the signatory's email may have been updated
              -- later.
              F.value "signatory_email" $ getEmail slink
            Nothing -> do
              -- signatory email: there are events that are missing affected
              -- signatory, but happen to have evEmail set to what we want
              F.value "signatory_email" $ evEmail dee
          -- This is terribad, but another possibility is to include it
          -- in DocumentEvidenceEvent or to include it in SignatoryLink
          -- and none of them are better. The best thing is to think how
          -- to rework evidence log module so that stuff like that can
          -- be somehow painlessly done, I guess.
          when (evType dee == Current SignDocumentEvidence) $ do
            dbQuery (GetESignature slinkid) >>= \case
              Nothing   -> return ()
              Just esig -> case esig of
                LegacyBankIDSignature_{}                        -> return ()
                LegacyTeliaSignature_{}                         -> return ()
                LegacyNordeaSignature_{}                        -> return ()
                LegacyMobileBankIDSignature_{}                  -> return ()
                CGISEBankIDSignature_ CGISEBankIDSignature {..} -> do
                  F.value "provider_sebankid" True
                NetsNOBankIDSignature_ NetsNOBankIDSignature {..} -> do
                  F.value "provider_nobankid" True
                NetsDKNemIDSignature_ NetsDKNemIDSignature {..} -> do
                  F.value "provider_dknemid" True
                EIDServiceIDINSignature_ EIDServiceNLIDINSignature {..} -> do
                  F.value "provider_idin" True
                EIDServiceFITupasSignature_ _ -> do
                  F.value "provider_fitupas" True
                EIDServiceOnfidoSignature_ _ -> do
                  F.value "provider_onfido" True
          when (evType dee == Current AuthenticatedToViewEvidence) $ do
            dbQuery (GetEAuthenticationWithoutSession AuthenticationToView slinkid)
              >>= \case
                    Nothing   -> return ()
                    Just esig -> case esig of
                      CGISEBankIDAuthentication_ n -> do
                        F.value "provider_sebankid" True
                        F.value "signatory_name" $ cgisebidaSignatoryName n
                      NetsNOBankIDAuthentication_ n -> do
                        F.value "provider_nobankid" True
                        F.value "signatory_name" $ netsNOBankIDSignatoryName n
                        F.value "signatory_dob" $ netsNOBankIDDateOfBirth n
                      NetsDKNemIDAuthentication_ n -> do
                        F.value "provider_dknemid" True
                        F.value "signatory_name" $ netsDKNemIDSignatoryName n
                        F.value "signatory_dob" $ netsDKNemIDDateOfBirth n
                      NetsFITupasAuthentication_ n -> do
                        F.value "provider_fitupas" True
                        F.value "signatory_name" $ netsFITupasSignatoryName n
                        F.value "signatory_dob" $ netsFITupasDateOfBirth n
                      SMSPinAuthentication_ mobile -> do
                        F.value "provider_sms_pin" True
                        F.value "signatory_mobile" mobile
                      EIDServiceVerimiAuthentication_ n -> do
                        F.value "provider_verimi" True
                        F.value "signatory_email" $ eidServiceVerimiVerifiedEmail n
                        F.value "signatory_mobile" $ eidServiceVerimiVerifiedPhone n
                      EIDServiceIDINAuthentication_ n -> do
                        F.value "provider_idin" True
                        F.value "signatory_mobile" $ eidServiceIDINVerifiedPhone n
                        F.value "provider_customer_id" $ eidServiceIDINCustomerID n
                      EIDServiceNemIDAuthentication_ n -> do
                        F.value "provider_dknemid" True
                        F.value "signatory_name" $ eidServiceNemIDSignatoryName n
                        F.value "signatory_dob" $ eidServiceNemIDDateOfBirth n
                      EIDServiceNOBankIDAuthentication_ n -> do
                        F.value "provider_nobankid" True
                        F.value "signatory_name" $ eidServiceNOBankIDSignatoryName n
                        F.value "signatory_dob" $ eidServiceNOBankIDDateOfBirth n
                      EIDServiceFITupasAuthentication_ n -> do
                        F.value "provider_fitupas" True
                        F.value "signatory_name" $ eidServiceFITupasSignatoryName n
                        F.value "signatory_dob" $ eidServiceFITupasDateOfBirth n
        F.value "text" $ T.replace "\n" " " <$> evMessageText dee -- Escape EOL. They are ignored by html and we don't want them on verification page
        F.value "additional_text" $ T.replace "\n" " " <$> evAdditionalMessageText dee -- Escape EOL. They are ignored by html and we don't want them on verification page
        F.value "signatory"
          $   (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder)
          <$> mslinkid
        F.forM_ mactor $ F.value "actor"

-- | Suppress repeated events stemming from mail delivery systems
-- reporting that an email was opened.  This is done by ignoring each
-- such event for five minutes after its last occurrence with the same
-- text.
suppressRepeatedEvents :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
suppressRepeatedEvents = go Map.empty  where
    go _ [] = []
    go levs (ev : evs)
      | evType ev == Current MarkInvitationReadEvidence
      = if Just (evTime ev) < ((5 `minutesAfter`) <$> Map.lookup (evText ev) levs)
        then go levs evs
        else ev : go (Map.insert (evText ev) (evTime ev) levs) evs
      | otherwise
      = ev : go levs evs
