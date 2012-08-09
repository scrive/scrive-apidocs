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

-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> Document -> [DocumentEvidenceEvent] -> m [JSValue] 
eventsJSListFromEvidenceLog tl crtime doc dees = mapM (J.runJSONGenT . eventJSValue tl crtime doc) $ eventsForLog doc dees


eventsForLog :: Document -> [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog _doc events =
    let
        sevents = filter (simpleEvents . evType)  events
        (es, es') = break (endOfHistoryEvent . evType) sevents
        separatedLog = es ++ (take 1 es')
        cleanerLog  = removeNotCriticalEventsWithSimillarTime separatedLog
    in cleanerLog

        
eventJSValue :: TemplatesMonad m => KontraTimeLocale -> MinutesTime ->  Document -> DocumentEvidenceEvent -> JSONGenT m ()
eventJSValue tl crtime doc dee = do
    J.value "time"  $ showDateAbbrev tl crtime (evTime dee)
    J.value "party" $ fromMaybe "Scrive" $ getSmartName <$> (getSigLinkFor doc $ evSigLinkID dee)
    J.valueM "text"  $ (simplyfiedEventText doc dee)


-- Removes events that seam to be duplicated
simpleEvents :: EvidenceEventType -> Bool
simpleEvents NewDocumentEvidence          = True
simpleEvents CloseDocumentEvidence        = True
simpleEvents CancelDocumentEvidence       = True
simpleEvents RejectDocumentEvidence       = True
simpleEvents TimeoutDocumentEvidence      = True
simpleEvents PreparationToPendingEvidence = True
simpleEvents MarkInvitationReadEvidence   = True
simpleEvents MarkDocumentSeenEvidence     = True
simpleEvents RestartDocumentEvidence      = True
simpleEvents _                            = False

endOfHistoryEvent :: EvidenceEventType -> Bool
endOfHistoryEvent  NewDocumentEvidence     = True
endOfHistoryEvent  RestartDocumentEvidence = True
endOfHistoryEvent _                        = False


notCriticalEvent :: EvidenceEventType -> Bool
notCriticalEvent MarkDocumentSeenEvidence   = True
notCriticalEvent MarkInvitationReadEvidence = True
notCriticalEvent _                          = False


simillarEvent :: DocumentEvidenceEvent -> DocumentEvidenceEvent -> Bool
simillarEvent e1 e2 =
        (evType e1 == evType e2)
     && (evSigLinkID e1 == evSigLinkID e2)
     && simillarEvent' (evType e1) (abs $ (toSeconds $ evTime e1) - (toSeconds $ evTime e2))
    where
        simillarEvent' MarkDocumentSeenEvidence tdiff = tdiff < 300
        simillarEvent' MarkInvitationReadEvidence _ = True
        simillarEvent' _ _ = False
        
removeNotCriticalEventsWithSimillarTime ::  [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
removeNotCriticalEventsWithSimillarTime [] = []
removeNotCriticalEventsWithSimillarTime (e:es) =
    if ((isNothing $ find (simillarEvent e) es) || not (notCriticalEvent $ evType e))
       then e : (removeNotCriticalEventsWithSimillarTime es)
       else (removeNotCriticalEventsWithSimillarTime es)


simplyfiedEventText :: TemplatesMonad m => Document -> DocumentEvidenceEvent -> m String
simplyfiedEventText doc dee = renderTemplate ("simpliefiedText" ++ (show $ evType dee)) $ do
    F.value "documenttitle" $ (documenttitle doc)

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



