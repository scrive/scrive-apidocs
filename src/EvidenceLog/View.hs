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

-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog ::  (MonadDB m, TemplatesMonad m) => KontraTimeLocale -> Document -> [DocumentEvidenceEvent] -> m [JSValue]
eventsJSListFromEvidenceLog tl doc dees = mapM (J.runJSONGenT . eventJSValue tl doc) $ eventsForLog doc dees


eventsForLog :: Document -> [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog _doc events =
    let
        sevents = filter (simpleEvents . evType)  events
        (es, es') = break (endOfHistoryEvent . evType) sevents
        separatedLog = es ++ (take 1 es')
        cleanerLog  = cleanSeenAfterSigned $ flattenSimilar separatedLog
    in cleanerLog

        
eventJSValue :: (MonadDB m, TemplatesMonad m) => KontraTimeLocale -> Document -> DocumentEvidenceEvent -> JSONGenT m ()
eventJSValue tl doc dee = do
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
simpleEvents MarkDocumentSeenEvidence     = True
simpleEvents RestartDocumentEvidence      = True
simpleEvents SignDocumentEvidence         = True
simpleEvents InvitationEvidence           = True
simpleEvents InvitationDelivered          = True
simpleEvents InvitationUndelivered        = True
simpleEvents ReminderSend                 = True
simpleEvents ResealedPDF                  = True
simpleEvents CancelDocumenElegEvidence    = True
simpleEvents _                            = False

-- Clean some events that should not be shown. Like reading docume by author after hi signed document.
cleanSeenAfterSigned :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
cleanSeenAfterSigned [] = []
cleanSeenAfterSigned (e:es) = if (evType e == MarkDocumentSeenEvidence && isJust (find (\e' -> evType e' == SignDocumentEvidence && evSigLinkID e' == evSigLinkID e) es))
                                 then cleanSeenAfterSigned es
                                 else (e:cleanSeenAfterSigned es)
                                

endOfHistoryEvent :: EvidenceEventType -> Bool
endOfHistoryEvent  PreparationToPendingEvidence     = True
endOfHistoryEvent _                        = False

-- Events that should be considered as performed as author even is actor states different.
authorEvents  :: EvidenceEventType -> Bool
authorEvents PreparationToPendingEvidence = True
authorEvents _ = False

-- Events that should be considered as performed as author even is actor states different.
systemEvents  :: EvidenceEventType -> Bool
systemEvents InvitationDelivered = True
systemEvents InvitationUndelivered = True
systemEvents _ = False



simillarEvent :: DocumentEvidenceEvent -> DocumentEvidenceEvent -> Bool
simillarEvent e1 e2 =
        (evType e1 == evType e2)
     && (evSigLinkID e1 == evSigLinkID e2)
     && simillarEvent' (evType e1)
    where
        simillarEvent' MarkDocumentSeenEvidence     = True
        simillarEvent' MarkInvitationReadEvidence  = True
        simillarEvent' _ = False

flattenSimilar ::  [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
flattenSimilar [] = []
flattenSimilar (e:es) =
    if ((isNothing $ find (simillarEvent e) es))
       then e : (flattenSimilar es)
       else (flattenSimilar es)


simplyfiedEventText :: TemplatesMonad m => Document -> DocumentEvidenceEvent -> m String
simplyfiedEventText doc dee = renderTemplate ("simpliefiedText" ++ (show $ evType dee)) $ do
    F.value "documenttitle" $ (documenttitle doc)
    case (evAffectedSigLinkID dee) of
      Just aslid -> F.value "affectedsignatory" $ getSmartName <$> getSigLinkFor doc aslid
      _ -> return ()
    F.value "text" $ evMessageText dee
    
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



