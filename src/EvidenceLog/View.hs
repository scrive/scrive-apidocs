module EvidenceLog.View (
      eventsJSListFromEvidenceLog
    , eventsForLog
    , getSignatoryLinks
    , simplyfiedEventText
    , approximateActor
    , htmlDocFromEvidenceLog
    , suppressRepeatedEvents
    , htmlSkipedEvidenceType
    , evidenceOfIntentHTML
  ) where


import qualified Data.Set as Set
import Doc.DocStateData
import Doc.Model (GetDocumentsBySignatoryLinkIDs(..))
import Doc.SignatoryLinkID (SignatoryLinkID)
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
import Data.Word (Word8)
import qualified HostClock.Model as HC
import User.Model
import DB
import Control.Logic

-- | Evidence log for web page - short and simplified texts
eventsJSListFromEvidenceLog ::  (MonadDB m, TemplatesMonad m) => Document -> [DocumentEvidenceEvent] -> m [JSValue]
eventsJSListFromEvidenceLog doc dees = mapM (J.runJSONGenT . eventJSValue doc) =<< getSignatoryLinks (eventsForLog dees)

-- | Lookup signatory links in events
getSignatoryLinks :: MonadDB m => [DocumentEvidenceEvent' SignatoryLinkID] -> m [DocumentEvidenceEvent' SignatoryLink]
getSignatoryLinks evs = do
  let docids = Set.fromList $ catMaybes $ map evSigLink evs ++ map evAffectedSigLink evs
  docs <- dbQuery $ GetDocumentsBySignatoryLinkIDs $ Set.toList docids
  let slmap = Map.fromList [(signatorylinkid s, s) | d <- docs, s <- documentsignatorylinks d]
      findsiglink s = flip Map.lookup slmap =<< s
  return $ for evs $ \ev -> ev { evSigLink = findsiglink (evSigLink ev)
                               , evAffectedSigLink = findsiglink (evAffectedSigLink ev)
                               }

-- | Keep only simple events, remove some redundant signatory events after signing
eventsForLog :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
eventsForLog = cleanUnimportantAfterSigning . filter ((simpleEvents . evType) &&^ (not . emptyEvent))

-- TODO: Consider saving actor name in event instead
approximateActor :: (MonadDB m, TemplatesMonad m) => Document -> DocumentEvidenceEvent' SignatoryLink -> m String
approximateActor doc dee | systemEvents $ evType dee = return "Scrive"
                         | otherwise =
  case evSigLink dee of
    Just sl -> if (isAuthor sl)
             then authorName
             else case (getSmartName sl) of
                    "" -> renderTemplate_ "_notNamedParty"
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

  where authorName = case (getAuthorSigLink doc) of
                        Just sl -> return $ getSmartName sl
                        Nothing -> renderTemplate_ "_authorParty"

eventJSValue :: (MonadDB m, TemplatesMonad m) => Document -> DocumentEvidenceEvent' SignatoryLink -> JSONGenT m ()
eventJSValue doc dee = do
    J.value "status" $ show $ getEvidenceEventStatusClass (evType dee)
    J.value "time"  $ formatMinutesTimeRealISO (evTime dee)
    J.valueM "party" $ approximateActor doc dee
    J.valueM "text"  $ simplyfiedEventText Nothing dee

-- | Simple events to be included in the archive history and the verification page.  These have translations.
simpleEvents :: EvidenceEventType -> Bool
simpleEvents (Current AttachExtendedSealedFileEvidence)  = True
simpleEvents (Current AttachGuardtimeSealedFileEvidence) = True
simpleEvents (Current CancelDocumenElegEvidence)         = True
simpleEvents (Current CancelDocumentEvidence)            = True
simpleEvents (Current CloseDocumentEvidence)             = True
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
simpleEvents (Current ResealedPDF)                       = True
simpleEvents (Current RestartDocumentEvidence)           = True
simpleEvents (Current SignDocumentEvidence)              = True
simpleEvents (Current SignatoryLinkVisited)              = True
simpleEvents (Current TimeoutDocumentEvidence)           = True
simpleEvents _                                           = False

getEvidenceEventStatusClass :: EvidenceEventType -> StatusClass
getEvidenceEventStatusClass (Current CloseDocumentEvidence)             = SCSigned
getEvidenceEventStatusClass (Current CancelDocumentEvidence)            = SCCancelled
getEvidenceEventStatusClass (Current RejectDocumentEvidence)            = SCRejected
getEvidenceEventStatusClass (Current TimeoutDocumentEvidence)           = SCTimedout
getEvidenceEventStatusClass (Current PreparationToPendingEvidence)      = SCSent
getEvidenceEventStatusClass (Current MarkInvitationReadEvidence)        = SCRead
getEvidenceEventStatusClass (Current SignatoryLinkVisited)              = SCOpened
getEvidenceEventStatusClass (Current RestartDocumentEvidence)           = SCDraft
getEvidenceEventStatusClass (Current SignDocumentEvidence)              = SCSigned
getEvidenceEventStatusClass (Current InvitationEvidence)                = SCSent
getEvidenceEventStatusClass (Current InvitationDeliveredByEmail)        = SCDelivered
getEvidenceEventStatusClass (Current InvitationUndeliveredByEmail)      = SCDeliveryProblem
getEvidenceEventStatusClass (Current InvitationDeliveredBySMS)          = SCDelivered
getEvidenceEventStatusClass (Current InvitationUndeliveredBySMS)        = SCDeliveryProblem
getEvidenceEventStatusClass (Current ReminderSend)                      = SCSent
getEvidenceEventStatusClass (Current ResealedPDF)                       = SCSigned
getEvidenceEventStatusClass (Current CancelDocumenElegEvidence)         = SCCancelled
getEvidenceEventStatusClass (Current ProlongDocumentEvidence)           = SCProlonged
getEvidenceEventStatusClass (Current AttachGuardtimeSealedFileEvidence) = SCSealed
getEvidenceEventStatusClass (Current AttachExtendedSealedFileEvidence)  = SCExtended
getEvidenceEventStatusClass _                                           = SCError

-- Remove signatory events that happen after signing (link visited, invitation read)
cleanUnimportantAfterSigning :: [DocumentEvidenceEvent] -> [DocumentEvidenceEvent]
cleanUnimportantAfterSigning = go Set.empty
  where go _ [] = []
        go m (e:es) | evType e `elem` [Current SignatoryLinkVisited, Current MarkInvitationReadEvidence]
                       && any (`Set.member` m) (ids e)
                    = go m es
                    | evType e == Current SignDocumentEvidence
                    = e : go (m `Set.union` Set.fromList (ids e)) es
                    | evType e == Current PreparationToPendingEvidence
                    = e : go Set.empty es
                    | otherwise
                    = e : go m es
        ids e = catMaybes [Left <$> evUserID e, Right <$> evSigLink e]

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

simplyfiedEventText :: TemplatesMonad m => Maybe String -> DocumentEvidenceEvent' SignatoryLink -> m String
simplyfiedEventText mactor dee = case evType dee of
  Obsolete _ -> return ""
  Current et -> renderTemplateI (eventTextTemplateName et) $ do
    let siglink = evAffectedSigLink dee
    F.value "text" $ filterTags <$> evMessageText dee
    F.value "signatory" $ getSmartName <$> siglink
    case mactor of
      Nothing    -> F.value "archive" True
      Just actor -> F.value "actor" actor
    maybe (return ()) signatoryLinkTemplateFields siglink

showClockError :: Word8 -> Double -> String
showClockError decimals e = show (realFracToDecimal decimals (e * 1000)) ++ " ms"

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
htmlSkipedEvidenceType (Obsolete OldDocumentHistory) = True
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
