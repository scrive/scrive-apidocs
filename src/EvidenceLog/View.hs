module EvidenceLog.View (
      eventsJSListFromEvidenceLog
    , htmlDocFromEvidenceLog
    , suppressRepeatedEvents
    , htmlSkipedEvidenceType
    , evidenceOfIntentHTML
  ) where


import Doc.DocStateData
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import qualified Doc.Screenshot as Screenshot
import Text.StringTemplates.Templates

import Control.Applicative

import MinutesTime
import Text.JSON

import Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F
import EvidenceLog.Model
import Utils.Prelude
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.Char8 as BS
import Data.Decimal (realFracToDecimal)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Word (Word8)
import qualified HostClock.Model as HC
import User.Model
import DB
import Control.Logic
import qualified Log

-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog ::  (MonadDB m, TemplatesMonad m) => Document -> [DocumentEvidenceEvent] -> m [JSValue]
eventsJSListFromEvidenceLog doc dees = mapM (J.runJSONGenT . eventJSValue doc) $ eventsForLog doc dees


eventsForLog :: Document -> [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog _doc events =
    let
        sevents = filter ((simpleEvents . evType) &&^ (not . emptyEvent))  events
        (es, es') = break (endOfHistoryEvent . evType) sevents
        separatedLog = es ++ (take 1 es')
        cleanerLog  = cleanUnimportantAfterSigning separatedLog
    in cleanerLog


eventJSValue :: (MonadDB m, TemplatesMonad m) => Document -> DocumentEvidenceEvent -> JSONGenT m ()
eventJSValue doc dee = do
    J.value "status" $ show $ getEvidenceEventStatusClass (evType dee)
    J.value "time"  $ formatMinutesTimeRealISO (evTime dee)
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
simpleEvents InvitationDeliveredByEmail   = True
simpleEvents InvitationUndeliveredByEmail = True
simpleEvents InvitationDeliveredBySMS     = True
simpleEvents InvitationUndeliveredBySMS   = True
simpleEvents ReminderSend                 = True
simpleEvents ResealedPDF                  = True
simpleEvents CancelDocumenElegEvidence    = True
simpleEvents ProlongDocumentEvidence      = True
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
getEvidenceEventStatusClass InvitationDeliveredByEmail   = SCDelivered
getEvidenceEventStatusClass InvitationUndeliveredByEmail = SCDeliveryProblem
getEvidenceEventStatusClass InvitationDeliveredBySMS     = SCDelivered
getEvidenceEventStatusClass InvitationUndeliveredBySMS   = SCDeliveryProblem
getEvidenceEventStatusClass ReminderSend                 = SCSent
getEvidenceEventStatusClass ResealedPDF                  = SCSigned
getEvidenceEventStatusClass CancelDocumenElegEvidence    = SCCancelled
getEvidenceEventStatusClass ProlongDocumentEvidence      = SCProlonged
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
systemEvents InvitationDeliveredByEmail = True
systemEvents InvitationUndeliveredByEmail = True
systemEvents InvitationDeliveredBySMS = True
systemEvents InvitationUndeliveredBySMS = True
systemEvents _ = False

-- Empty events - they should be skipped, as they don't provide enought information to show to user
emptyEvent :: DocumentEvidenceEvent -> Bool
emptyEvent (DocumentEvidenceEvent {evType = InvitationEvidence, evAffectedSigLinkID = Nothing }) = True
emptyEvent (DocumentEvidenceEvent {evType = ReminderSend,       evAffectedSigLinkID = Nothing }) = True
emptyEvent _ = False

simplyfiedEventText :: (TemplatesMonad m, Log.MonadLog m) => Document -> DocumentEvidenceEvent -> m String
simplyfiedEventText doc dee = renderTemplate ("simpliefiedText" ++ (show $ evType dee)) $ do
    let siglink = (getSigLinkFor doc =<< evAffectedSigLinkID dee)
    F.value "documenttitle" $ (documenttitle doc)
    F.value "affectedsignatory" $ getSmartName <$> siglink
    F.value "text" $ filterTags <$> evMessageText dee
    F.value "eleg"   $ (\sl -> signatorylinkauthenticationmethod sl == ELegAuthentication) <$> siglink
    F.value "pad"    $ (\sl -> signatorylinkdeliverymethod sl == PadDelivery) <$> siglink
    F.value "email"  $ (\sl -> signatorylinkdeliverymethod sl == EmailDelivery) <$> siglink
    F.value "mobile" $ (\sl -> signatorylinkdeliverymethod sl == MobileDelivery) <$> siglink
    F.value "emailmobile" $ (\sl -> signatorylinkdeliverymethod sl == EmailAndMobileDelivery) <$> siglink
    F.value "viewing" $ (not . signatoryispartner) <$> siglink

showClockError :: Word8 -> Double -> String
showClockError decimals e = show (realFracToDecimal decimals (e * 1000)) ++ " ms"

-- | Suppress repeated events stemming from mail delivery systems
-- reporting that an email was opened.  This is done by ignoring each
-- such event for five minutes after its last occurrence with the same
-- text.
suppressRepeatedEvents :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
suppressRepeatedEvents = go Map.empty where
  go _ [] = []
  go levs (ev:evs) | evType ev == MarkInvitationReadEvidence = if Just (evTime ev) < ((5 `minutesAfter`) <$> Map.lookup (evText ev) levs)
                                                               then go levs evs
                                                               else ev : go (Map.insert (evText ev) (evTime ev) levs) evs
                  | otherwise = ev : go levs evs

-- | Generating text of Evidence log that is attachmed to PDF. It should be complete
htmlDocFromEvidenceLog :: TemplatesMonad m => String -> [DocumentEvidenceEvent] -> HC.ClockErrorStatistics -> m String
htmlDocFromEvidenceLog title elog ces = do
  renderTemplate "htmlevidencelog" $ do
    F.value "documenttitle" title
    F.value "ce_max"       $ showClockError 1 <$> HC.max ces
    F.value "ce_mean"      $ showClockError 1 <$> HC.mean ces
    F.value "ce_std_dev"   $ showClockError 1 <$> HC.std_dev ces
    F.value "ce_collected" $ HC.collected ces
    F.value "ce_missed"    $ HC.missed ces
    F.objects "entries" $ for (filter (not . htmlSkipedEvidenceType . evType) elog) $ \entry -> do
      F.value "time" $ formatMinutesTimeUTC (evTime entry) ++ " UTC"
                       ++ maybe "" (\e -> " ±" ++ showClockError 0 e)
                                   (HC.maxClockError (evTime entry) <$> evClockErrorEstimate entry)
      F.value "ces_time" $ maybe "" ((++" UTC") . formatMinutesTimeUTC . HC.time)
                                    (evClockErrorEstimate entry)
      F.value "ip"   $ show <$> evIP4 entry
      F.value "text" $ evText entry

htmlSkipedEvidenceType :: EvidenceEventType -> Bool
htmlSkipedEvidenceType OldDocumentHistory = True
htmlSkipedEvidenceType _ = False

filterTags :: String -> String
filterTags ('<':rest) = ' ' : (filterTags (drop 1 $ dropWhile (\c -> c /= '>') rest))
filterTags (a:rest) = a : (filterTags rest)
filterTags [] = []

-- | Detect one of 'png' or 'jpeg' based on magic numbers in binary content.
detectImageMimeType :: BS.ByteString -> BS.ByteString
detectImageMimeType bs =
  case () of
    _ | BS.take 4 bs == BS.pack "\xff\xd8\xff\xe0"  -> BS.pack "image/jpeg"
    _ | BS.take 4 bs == BS.pack "\x89\x50\x4e\x47"  -> BS.pack "image/png"
    _ -> "application/octet-stream"

-- | Generate evidence of intent in self-contained HTML for inclusion as attachment in PDF.
evidenceOfIntentHTML :: TemplatesMonad m => String -> [(SignatoryLink, SignatoryScreenshots.SignatoryScreenshots)] -> m String
evidenceOfIntentHTML title l = do
  renderTemplate "evidenceOfIntent" $ do
    F.value "documenttitle" title
    let values Nothing = return ()
        values (Just (t,s)) = do
          F.value "time" $ formatMinutesTimeUTC t ++ " UTC"
          F.value "image" $ RFC2397.encode (detectImageMimeType (unBinary (Screenshot.image s)))
                                           (unBinary (Screenshot.image s))
    F.objects "entries" $ for l $ \(sl, entry) -> do
      F.value "signatory"  $ getIdentifier sl
      F.value "ip"         $ show . signipnumber <$> maybesigninfo sl
      F.object "first"     $ values (SignatoryScreenshots.first entry)
      F.object "signing"   $ values (SignatoryScreenshots.signing entry)
      F.object "reference" $ values (Just (SignatoryScreenshots.reference entry))
