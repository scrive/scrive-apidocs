module EvidenceLog.View (
      eventsJSListFromEvidenceLog
    , htmlDocFromEvidenceLog
    , htmlSkipedEvidenceType
  ) where


import Doc.DocStateData
import Templates.Templates

import Control.Applicative

import MinutesTime
import Text.JSON

import Text.JSON.Gen as J
import qualified Templates.Fields as F
import EvidenceLog.Model
import Utils.Prelude
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Data.Maybe
import Data.List
import User.Model
import DB
import Control.Logic
-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog ::  (MonadDB m, TemplatesMonad m) => KontraTimeLocale -> Document -> [DocumentEvidenceEvent] -> m [JSValue]
eventsJSListFromEvidenceLog tl doc dees = mapM (J.runJSONGenT . eventJSValue tl doc) $ eventsForLog doc dees


eventsForLog :: Document -> [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog _doc events =
    let
        sevents = filter ((simpleEvents . evType) &&^ (not . emptyEvent))  events
        (es, es') = break (endOfHistoryEvent . evType) sevents
        separatedLog = es ++ (take 1 es')
        cleanerLog  = cleanUnimportantAfterSigning separatedLog
    in cleanerLog

        
eventJSValue :: (MonadDB m, TemplatesMonad m) => KontraTimeLocale -> Document -> DocumentEvidenceEvent -> JSONGenT m ()
eventJSValue tl doc dee = do
    J.value "status" $ show $ getEvidenceEventStatusClass (evType dee)
    J.value "time"  $ showDateForHistory tl (evTime dee)
    J.valueM "party" $ if (systemEvents $ evType dee)
                          then return "Scrive"
                          else case (getSigLinkFor doc $ evSigLinkID dee) of
                                Just sl -> if (isAuthor sl)
                                         then authorName
                                         else case (getSmartName sl) of
                                                "" -> renderTemplate_ "notNamedParty"
                                                name -> return name
                                Nothing -> case (evUserID dee) of                       
                                           Just uid -> if (isAuthor (doc,uid))
                                                        then authorName
                                                        else do
                                                          muser <- dbQuery $ GetUserByID uid
                                                          case muser of
                                                            Just user -> return $ getSmartName user
                                                            _ -> return "Scrive" -- This should not happend
                                           _ ->  if (authorEvents $ evType dee)
                                                    then authorName
                                                    else return "Scrive"
    J.valueM "text"  $ (simplyfiedEventText doc dee)
  where authorName = case (getAuthorSigLink doc) of
                        Just sl -> return $ getSmartName sl
                        Nothing -> renderTemplate_ "authorParty"



-- Removes events that seam to be duplicated
simpleEvents :: EvidenceEventType -> Bool
simpleEvents NewDocumentEvidence          = True
simpleEvents CloseDocumentEvidence        = True
simpleEvents CancelDocumentEvidence       = True
simpleEvents RejectDocumentEvidence       = True
simpleEvents TimeoutDocumentEvidence      = True
simpleEvents PreparationToPendingEvidence = True
simpleEvents MarkInvitationReadEvidence   = True
simpleEvents SignatoryLinkVisited         = True
simpleEvents RestartDocumentEvidence      = True
simpleEvents SignDocumentEvidence         = True
simpleEvents InvitationEvidence           = True
simpleEvents InvitationDelivered          = True
simpleEvents InvitationUndelivered        = True
simpleEvents ReminderSend                 = True
simpleEvents ResealedPDF                  = True
simpleEvents CancelDocumenElegEvidence    = True
simpleEvents _                            = False

getEvidenceEventStatusClass :: EvidenceEventType -> StatusClass
getEvidenceEventStatusClass NewDocumentEvidence          = SCDraft
getEvidenceEventStatusClass CloseDocumentEvidence        = SCSigned
getEvidenceEventStatusClass CancelDocumentEvidence       = SCCancelled
getEvidenceEventStatusClass RejectDocumentEvidence       = SCRejected
getEvidenceEventStatusClass TimeoutDocumentEvidence      = SCTimedout
getEvidenceEventStatusClass PreparationToPendingEvidence = SCSent
getEvidenceEventStatusClass MarkInvitationReadEvidence   = SCRead
getEvidenceEventStatusClass SignatoryLinkVisited         = SCOpened
getEvidenceEventStatusClass RestartDocumentEvidence      = SCDraft
getEvidenceEventStatusClass SignDocumentEvidence         = SCSigned
getEvidenceEventStatusClass InvitationEvidence           = SCSent
getEvidenceEventStatusClass InvitationDelivered          = SCDelivered
getEvidenceEventStatusClass InvitationUndelivered        = SCDeliveryProblem
getEvidenceEventStatusClass ReminderSend                 = SCSent
getEvidenceEventStatusClass ResealedPDF                  = SCSigned
getEvidenceEventStatusClass CancelDocumenElegEvidence    = SCCancelled
getEvidenceEventStatusClass _                            = SCError

-- Clean some events that should not be shown. Like reading after signing.
cleanUnimportantAfterSigning :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
cleanUnimportantAfterSigning [] = []
cleanUnimportantAfterSigning (e:es) = if ((     (evType e == SignatoryLinkVisited)
                                           || (evType e == MarkInvitationReadEvidence))
                                        && wasSigned (evUserID e, evSigLinkID e) es)
                                        then cleanUnimportantAfterSigning es
                                        else (e:cleanUnimportantAfterSigning es)
  where wasSigned (muid,mslid) es' =
          isJust (find (\e' ->     evType e' == SignDocumentEvidence
                                && ((isJust muid && evUserID e' == muid)
                                     || (isJust mslid && evSigLinkID e' == mslid)
                                    )) es')
                                

endOfHistoryEvent :: EvidenceEventType -> Bool
endOfHistoryEvent  PreparationToPendingEvidence = True
endOfHistoryEvent  NewDocumentEvidence          = True
endOfHistoryEvent  RestartDocumentEvidence      = True
endOfHistoryEvent _                             = False

-- Events that should be considered as performed as author even is actor states different.
authorEvents  :: EvidenceEventType -> Bool
authorEvents PreparationToPendingEvidence = True
authorEvents _ = False

-- Events that should be considered as performed as author even is actor states different.
systemEvents  :: EvidenceEventType -> Bool
systemEvents InvitationDelivered = True
systemEvents InvitationUndelivered = True
systemEvents _ = False

-- Empty events - they should be skipped, as they don't provide enought information to show to user
emptyEvent :: DocumentEvidenceEvent -> Bool
emptyEvent (DocumentEvidenceEvent {evType = InvitationEvidence, evAffectedSigLinkID = Nothing }) = True
emptyEvent (DocumentEvidenceEvent {evType = ReminderSend,       evAffectedSigLinkID = Nothing }) = True
emptyEvent _ = False

simplyfiedEventText :: TemplatesMonad m => Document -> DocumentEvidenceEvent -> m String
simplyfiedEventText doc dee = renderTemplate ("simpliefiedText" ++ (show $ evType dee)) $ do
    F.value "documenttitle" $ (documenttitle doc)
    case (evAffectedSigLinkID dee) of
      Just aslid -> F.value "affectedsignatory" $ getSmartName <$> getSigLinkFor doc aslid
      Nothing    -> F.value "affectedsignatory" ("" :: String)
    F.value "text" $ filterTags <$> evMessageText dee
    F.value "eleg" $ documentauthenticationmethod doc == ELegAuthentication
    F.value "pad"  $ documentdeliverymethod doc == PadDelivery
    
-- | Generating text of Evidence log that is attachmed to PDF. It should be compleate
htmlDocFromEvidenceLog :: TemplatesMonad m => String -> [DocumentEvidenceEvent] -> m String
htmlDocFromEvidenceLog title elog = do
  renderTemplate "htmlevidencelog" $ do
    F.value "documenttitle" title
    F.objects "entries" $ for (filter (not . htmlSkipedEvidenceType . evType) elog) $ \entry -> do
      F.value "time" $ formatMinutesTimeUTC (evTime entry) ++ " UTC"
      F.value "ip"   $ show <$> evIP4 entry
      F.value "text" $ evText entry

htmlSkipedEvidenceType :: EvidenceEventType -> Bool
htmlSkipedEvidenceType OldDocumentHistory = True
htmlSkipedEvidenceType _ = False

filterTags :: String -> String
filterTags ('<':rest) = ' ' : (filterTags (drop 1 $ dropWhile (\c -> c /= '>') rest))
filterTags (a:rest) = a : (filterTags rest)
filterTags [] = []

