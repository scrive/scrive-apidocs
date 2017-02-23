module EvidenceLog.View (
      eventsJSListFromEvidenceLog
    , eventsForLog
    , getSignatoryIdentifierMap
    , simplyfiedEventText
    , approximateActor
    , suppressRepeatedEvents
    , simpleEvents
    , eventForHistory
    , eventForVerificationPage
  ) where

import Control.Monad.Catch
import Data.Function (on)
import Data.String.Utils as String
import Text.JSON
import Text.JSON.Gen as J
import Text.StringTemplates.Templates
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.StringTemplates.Fields as F

import DB
import Doc.DocStateData
import Doc.Model (GetDocumentsBySignatoryLinkIDs(..))
import Doc.SignatoryIdentification (SignatoryIdentifierMap, siFullName, siLink, signatoryIdentifier, signatoryIdentifierMap)
import EID.Authentication.Model
import EID.Nets.Data
import EID.Signature.Model
import EvidenceLog.Model
import KontraPrelude
import MinutesTime
import Templates (renderLocalTemplate)
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog ::  (MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> [DocumentEvidenceEvent] -> m [JSValue]
eventsJSListFromEvidenceLog doc dees = do
  let evs = filter (eventForHistory . evType) $ eventsForLog dees
  sim <- getSignatoryIdentifierMap True evs
  mapM (J.runJSONGenT . eventJSValue doc sim) evs

-- | Get signatory identifier map from event list
getSignatoryIdentifierMap :: (MonadDB m, MonadThrow m) => Bool -> [DocumentEvidenceEvent] -> m SignatoryIdentifierMap
getSignatoryIdentifierMap includeviewers evs = do
  let sigs = Set.fromList $ catMaybes $ concat [ [evSigLink ev, evAffectedSigLink ev] | ev <- evs ]
  docs <- dbQuery $ GetDocumentsBySignatoryLinkIDs $ Set.toList sigs
  return $ signatoryIdentifierMap includeviewers (sortBy (compare `on` documentid) docs) sigs

-- | Keep only simple events, remove some redundant signatory events after signing
eventsForLog :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog = cleanUnimportantAfterSigning . filter ((simpleEvents . evType) && (not . emptyEvent))

-- TODO: Consider saving actor name in event instead, this is likely to become broken
approximateActor :: (MonadDB m, MonadThrow m, TemplatesMonad m) => EventRenderTarget -> Document -> SignatoryIdentifierMap -> DocumentEvidenceEvent -> m String
approximateActor EventForEvidenceLog _ _ _ = $unexpectedErrorM "approximateActor should not be called for evidence log entries"
approximateActor tgt doc sim dee | systemEvents $ evType dee = return "Scrive"
                             | otherwise = do
  emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
  case evSigLink dee >>= sigid emptyNamePlaceholder of
    Just i -> return i
    Nothing -> case evUserID dee of
               Just uid -> if (isAuthor (doc,uid))
                            then authorName emptyNamePlaceholder
                            else do
                              muser <- dbQuery $ GetUserByID uid
                              case muser of
                                Just user -> return $ getSmartName user ++ " (" ++ getEmail user ++ ")"
                                _ -> return "Scrive" -- This should not happend
               _ ->  if (authorEvents $ evType dee)
                        then authorName emptyNamePlaceholder
                        else return "Scrive"

  where authorName emptyNamePlaceholder = case getAuthorSigLink doc >>= sigid emptyNamePlaceholder . signatorylinkid of
                        Just i -> return i
                        Nothing -> renderTemplate_ "_authorParty"
        sigid emptyNamePlaceholder s | tgt == EventForArchive = do
                                             si <- Map.lookup s sim
                                             let name = siFullName si
                                             if null name then
                                                 signatoryIdentifier sim s emptyNamePlaceholder
                                              else
                                                 return name
                                     | otherwise = signatoryIdentifier sim s emptyNamePlaceholder

eventJSValue :: (MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryIdentifierMap -> DocumentEvidenceEvent -> JSONGenT m ()
eventJSValue doc sim dee = do
    J.value "status" $ show $ getEvidenceEventStatusClass (evType dee)
    J.value "time"   $ formatTimeISO (evTime dee)
    J.valueM "party" $ approximateActor EventForArchive doc sim dee
    J.valueM "text"  $ simplyfiedEventText EventForArchive Nothing doc sim dee

-- | Simple events to be included in the archive history and the verification page.  These have translations.
simpleEvents :: EvidenceEventType -> Bool
simpleEvents (Current AttachExtendedSealedFileEvidence)  = True
simpleEvents (Current AttachGuardtimeSealedFileEvidence) = True
simpleEvents (Obsolete CancelDocumenElegEvidence)        = True
simpleEvents (Current CancelDocumentEvidence)            = True
simpleEvents (Current InvitationDeliveredByEmail)        = True
simpleEvents (Current InvitationDeliveredBySMS)          = True
simpleEvents (Current InvitationEvidence)                = True
simpleEvents (Current InvitationUndeliveredByEmail)      = True
simpleEvents (Current InvitationUndeliveredBySMS)        = True
simpleEvents (Current MarkInvitationReadEvidence)        = True
simpleEvents (Current PreparationToPendingEvidence)      = True
simpleEvents (Current ProlongDocumentEvidence)           = True
simpleEvents (Current RejectDocumentEvidence)            = True
simpleEvents (Current ReminderSend)                      = True
simpleEvents (Current AutomaticReminderSent)             = True
simpleEvents (Current RestartDocumentEvidence)           = True
simpleEvents (Current SignDocumentEvidence)              = True
simpleEvents (Obsolete SignatoryLinkVisited)              = True
simpleEvents (Current TimeoutDocumentEvidence)           = True
simpleEvents (Current SMSPinSendEvidence)                = True
simpleEvents (Current SMSPinDeliveredEvidence)           = True
simpleEvents (Current VisitedViewForAuthenticationEvidence) = True
simpleEvents (Current VisitedViewForSigningEvidence)     = True
simpleEvents (Current AuthenticatedToViewEvidence)       = True
simpleEvents (Current AuthorAttachmentAccepted)          = True
simpleEvents _                                           = False

getEvidenceEventStatusClass :: EvidenceEventType -> StatusClass
getEvidenceEventStatusClass (Current CloseDocumentEvidence)             = SCSigned
getEvidenceEventStatusClass (Current CancelDocumentEvidence)            = SCCancelled
getEvidenceEventStatusClass (Current RejectDocumentEvidence)            = SCRejected
getEvidenceEventStatusClass (Current TimeoutDocumentEvidence)           = SCTimedout
getEvidenceEventStatusClass (Current PreparationToPendingEvidence)      = SCInitiated
getEvidenceEventStatusClass (Current MarkInvitationReadEvidence)        = SCRead
getEvidenceEventStatusClass (Obsolete SignatoryLinkVisited)             = SCOpened
getEvidenceEventStatusClass (Current RestartDocumentEvidence)           = SCDraft
getEvidenceEventStatusClass (Current SignDocumentEvidence)              = SCSigned
getEvidenceEventStatusClass (Current InvitationEvidence)                = SCSent
getEvidenceEventStatusClass (Current InvitationDeliveredByEmail)        = SCDelivered
getEvidenceEventStatusClass (Current InvitationUndeliveredByEmail)      = SCDeliveryProblem
getEvidenceEventStatusClass (Current InvitationDeliveredBySMS)          = SCDelivered
getEvidenceEventStatusClass (Current InvitationUndeliveredBySMS)        = SCDeliveryProblem
getEvidenceEventStatusClass (Current ReminderSend)                      = SCSent
getEvidenceEventStatusClass (Current AutomaticReminderSent)             = SCSent
getEvidenceEventStatusClass (Current ResealedPDF)                       = SCSigned
getEvidenceEventStatusClass (Obsolete CancelDocumenElegEvidence)        = SCCancelled
getEvidenceEventStatusClass (Current ProlongDocumentEvidence)           = SCProlonged
getEvidenceEventStatusClass (Current AttachSealedFileEvidence)          = SCSigned
getEvidenceEventStatusClass (Current AttachGuardtimeSealedFileEvidence) = SCSealed
getEvidenceEventStatusClass (Current AttachExtendedSealedFileEvidence)  = SCExtended
getEvidenceEventStatusClass (Current SMSPinSendEvidence)                = SCSent
getEvidenceEventStatusClass (Current SMSPinDeliveredEvidence)           = SCDelivered
getEvidenceEventStatusClass (Current VisitedViewForAuthenticationEvidence) = SCOpenedAuthToView
getEvidenceEventStatusClass (Current VisitedViewForSigningEvidence)        = SCOpened
getEvidenceEventStatusClass (Current AuthenticatedToViewEvidence)          = SCAuthenticatedToView

getEvidenceEventStatusClass _                                           = SCError

-- Remove signatory events that happen after signing (link visited, invitation read)
cleanUnimportantAfterSigning :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
cleanUnimportantAfterSigning = go Set.empty
  where go _ [] = []
        go m (e:es) | evType e `elem` [Obsolete SignatoryLinkVisited, Current MarkInvitationReadEvidence]
                       && ids e `Set.member` m
                    = go m es -- the only place for skipping events, but these events always have evSigLink == Just ...
                    | evType e == Current SignDocumentEvidence
                    = e : go (Set.insert (ids e) m) es
                    | evType e == Current PreparationToPendingEvidence
                    = e : go Set.empty es
                    | otherwise
                    = e : go m es
        ids e = (evUserID e, evSigLink e)

-- Events that should be considered as performed as author even is actor states different.
authorEvents  :: EvidenceEventType -> Bool
authorEvents (Current PreparationToPendingEvidence) = True
authorEvents _ = False

-- Events that should be considered as performed by the system even if actor states different.
systemEvents  :: EvidenceEventType -> Bool
systemEvents (Current InvitationDeliveredByEmail) = True
systemEvents (Current InvitationUndeliveredByEmail) = True
systemEvents (Current InvitationDeliveredBySMS) = True
systemEvents (Current InvitationUndeliveredBySMS) = True
systemEvents _ = False

-- Empty events - they should be skipped, as they don't provide enought information to show to user
emptyEvent :: DocumentEvidenceEvent -> Bool
emptyEvent (DocumentEvidenceEvent {evType = Current InvitationEvidence, evAffectedSigLink = Nothing }) = True
emptyEvent (DocumentEvidenceEvent {evType = Current ReminderSend,       evAffectedSigLink = Nothing }) = True
emptyEvent _ = False

eventForVerificationPage :: EvidenceEventType -> Bool
eventForVerificationPage = not . (`elem` map Current [AttachGuardtimeSealedFileEvidence, AttachExtendedSealedFileEvidence, MarkInvitationReadEvidence])

eventForHistory :: EvidenceEventType -> Bool
eventForHistory = not . (`elem` map Current [AuthorAttachmentAccepted])

-- | Produce simplified text for an event (only for archive or
-- verification pages).
simplyfiedEventText :: (HasLang d, MonadDB m, MonadThrow m, TemplatesMonad m)
  => EventRenderTarget -> Maybe String -> d -> SignatoryIdentifierMap -> DocumentEvidenceEvent -> m String
simplyfiedEventText EventForEvidenceLog _ _ _ _ = $unexpectedErrorM "simplyfiedEventText should not be called for evidence log entries"
simplyfiedEventText target mactor d sim dee = do
  emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
  case evType dee of
    Obsolete CancelDocumenElegEvidence -> renderEvent emptyNamePlaceholder "CancelDocumenElegEvidenceText"
    Obsolete SignatoryLinkVisited -> renderEvent emptyNamePlaceholder "SignatoryLinkVisitedArchive"
    Current et -> renderEvent emptyNamePlaceholder $ eventTextTemplateName target et
    Obsolete _ -> return "" -- shouldn't we throw an error in this case?
    where
      render | target == EventForVerificationPages = renderLocalTemplate (getLang d)
             | otherwise                           = renderTemplate
      renderEvent emptyNamePlaceholder eventTemplateName = render eventTemplateName $ do
        let mslinkid = evAffectedSigLink dee
        F.forM_ mslinkid  $ \slinkid -> do
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
              Nothing -> return ()
              Just esig -> F.value "eid_signatory_name" $ case esig of
                LegacyBankIDSignature_{} -> Nothing
                LegacyTeliaSignature_{} -> Nothing
                LegacyNordeaSignature_{} -> Nothing
                LegacyMobileBankIDSignature_{} -> Nothing
                CGISEBankIDSignature_ CGISEBankIDSignature{..} -> Just cgisebidsSignatoryName
          when (evType dee == Current AuthenticatedToViewEvidence) $ do
            dbQuery (GetEAuthenticationWithoutSession slinkid) >>= \case
              Nothing -> return ()
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

        F.value "text" $ String.replace "\n" " " <$> evMessageText dee -- Escape EOL. They are ignored by html and we don't want them on verification page
        F.value "additional_text" $ String.replace "\n" " " <$> evAdditionalMessageText dee -- Escape EOL. They are ignored by html and we don't want them on verification page
        F.value "signatory" $ (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder) <$> mslinkid
        F.forM_ mactor $ F.value "actor"

-- | Suppress repeated events stemming from mail delivery systems
-- reporting that an email was opened.  This is done by ignoring each
-- such event for five minutes after its last occurrence with the same
-- text.
suppressRepeatedEvents :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
suppressRepeatedEvents = go Map.empty where
  go _ [] = []
  go levs (ev:evs) | evType ev == Current MarkInvitationReadEvidence =
                       if Just (evTime ev) < ((5 `minutesAfter`) <$> Map.lookup (evText ev) levs)
                       then go levs evs
                       else ev : go (Map.insert (evText ev) (evTime ev) levs) evs
                  | otherwise = ev : go levs evs
