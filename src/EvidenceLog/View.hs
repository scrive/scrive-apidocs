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
eventsJSListFromEvidenceLog ::  (MonadDB m, TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> Document -> [DocumentEvidenceEvent] -> m [JSValue] 
eventsJSListFromEvidenceLog tl crtime doc dees = mapM (J.runJSONGenT . eventJSValue tl crtime doc) $ eventsForLog doc dees


eventsForLog :: Document -> [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog _doc events =
    let
        sevents = filter (simpleEvents . evType)  events
        (es, es') = break (endOfHistoryEvent . evType) sevents
        separatedLog = es ++ (take 1 es')
        cleanerLog  = flattenSimilar separatedLog
    in cleanerLog

        
eventJSValue :: (MonadDB m, TemplatesMonad m) => KontraTimeLocale -> MinutesTime ->  Document -> DocumentEvidenceEvent -> JSONGenT m ()
eventJSValue tl crtime doc dee = do
    J.value "time"  $ showDateAbbrev tl crtime (evTime dee)
    J.valueM "party" $ case (getSigLinkFor doc $ evSigLinkID dee) of
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
simpleEvents _                            = False

endOfHistoryEvent :: EvidenceEventType -> Bool
endOfHistoryEvent  PreparationToPendingEvidence     = True
endOfHistoryEvent _                        = False

-- Events that should be considered as performed as author even is actor states different.
authorEvents  :: EvidenceEventType -> Bool
authorEvents PreparationToPendingEvidence = True
authorEvents _ = False



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



