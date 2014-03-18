module DumpEvidenceTexts (dumpAllEvidenceTexts) where

import Data.Decimal (realFracToDecimal)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust, isNothing)
import DB (MonadDB)
import Doc.DocStateData (SignatoryField(..), SignatoryLink(..), FieldType(..), DeliveryMethod(..))
import Doc.DocumentID (unsafeDocumentID)
import Doc.DocumentMonad (withDocumentID)
import EvidenceLog.View (simpleEvents, simplyfiedEventText, eventForVerificationPage)
import Text.StringTemplates.Templates (TemplatesMonad, renderTemplate)
import Util.Actor (Actor(..), actorEmail, actorUserID, actorAPIString, actorIP)
import Version (versionID)

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import MinutesTime
import TestKontra (TestEnvSt, teOutputDirectory, teGlobalTemplates)
import qualified Text.StringTemplates.Fields as F
import EvidenceLog.Model (DocumentEvidenceEvent'(..), EvidenceEventType(..), CurrentEvidenceEventType(..), evidenceLogText)
import System.FilePath ((</>))
import Templates (runTemplatesT)
import Test.Framework (Test)
import TestingUtil (testThat)
import Utils.Default (defaultValue)
import Utils.Prelude (for)
import User.Model (codeFromLang, Lang)

dumpAllEvidenceTexts :: TestEnvSt -> Test
dumpAllEvidenceTexts env = testThat "Generating all evidence texts" env $ do
  forM_ [minBound .. maxBound] $ \lang -> do
    gts <- asks teGlobalTemplates
    now <- getMinutesTime
    t <- runTemplatesT (lang, gts) $ dumpEvidenceTexts now lang
    case teOutputDirectory env of
      Just d  -> liftIO $ writeFile (d </> "evidence-texts-" ++ codeFromLang lang ++ ".html") t
      Nothing -> t == t `seq` return ()

dumpEvidenceTexts :: (MonadDB m, TemplatesMonad m) => MinutesTime -> Lang -> m String
dumpEvidenceTexts now lang = do
  let Just time = parseMinutesTimeDMY "01-01-2013"
  let actor = Actor { actorTime = time
                    , actorClientTime = Nothing
                    , actorClientName = Nothing
                    , actorIP = Nothing
                    , actorUserID = Nothing
                    , actorEmail = Just "author@example.com"
                    , actorSigLinkID = Nothing
                    , actorAPIString = Nothing
                    , actorWho = "the author (" ++ fromJust (actorEmail actor) ++ ")"
                    }
  let evidencetypes = [minBound .. maxBound]
  let sl = Just defaultValue
  let asl = Just defaultValue
            { signatoryfields =
                  [ SignatoryField FirstNameFT "Sven" True False []
                  , SignatoryField LastNameFT "Signatory" True False []
                  , SignatoryField EmailFT "signatory@example.com" True False []
                  ]
            , signatoryispartner = True
            , signatorylinkdeliverymethod = EmailAndMobileDelivery
            }
  let messageText = Just "This is a <b>message text.</b>"
  let fields t = do
        when (t `elem` [AutomaticReminderSent, ReminderSend, DeleteSigAttachmentEvidence, SaveSigAttachmentEvidence]) $ do
          F.value "author" $ actorEmail actor
        F.value "description" ("This is a description." :: String)
        F.value "lang" $ show lang
        F.value "msg" ("Really long message from external eID system." :: String)
        F.value "name" ("some name" :: String)
        F.value "newemail" ("good@example.com" :: String)
        F.value "newphone" ("good-12 34 56" :: String)
        F.value "oldemail" ("bad@example.com" :: String)
        F.value "oldphone" ("bad-12 34 56" :: String)
        F.value "timeouttime" $ formatMinutesTimeUTC time
        F.value "timezone" ("Europe/Stockholm" :: String)
        F.value "value" ("field value" :: String)
        F.value "fieldname" ("field name" :: String)
        F.objects "placements" $ for [(1::Int,0.123::Double,0.42::Double)] $ \(page,x,y) -> do
                       F.value "page" $ page
                       F.value "x" $ show $ realFracToDecimal 3 $ x
                       F.value "y" $ show $ realFracToDecimal 3 $ y
  let mkev text evt =
          DocumentEvidenceEvent { evDocumentID = unsafeDocumentID 0
                                , evTime = time
                                , evClientTime = Nothing
                                , evClientName = Nothing
                                , evClockErrorEstimate = Nothing
                                , evText = text
                                , evType = Current evt
                                , evVersionID = versionID
                                , evEmail = actorEmail actor
                                , evUserID = actorUserID actor
                                , evIP4 = actorIP actor
                                , evSigLink = sl
                                , evAPI = actorAPIString actor
                                , evAffectedSigLink = asl
                                , evMessageText = messageText
                                }
  evs <- (sortBy (compare `on` (\(evt, _, _, _) -> show evt)) <$>) $
         forM (evidencetypes) $ \evt -> do
       elog <- withDocumentID (unsafeDocumentID 0) $ evidenceLogText evt (fields evt) asl messageText actor
       let simpletext mactor = if simpleEvents (Current evt) && (isNothing mactor || eventForVerificationPage ev)
                               then Just <$> simplyfiedEventText mactor lang ev
                               else return Nothing
              where ev = mkev elog evt
       vp <- simpletext (actorEmail actor)
       av <- simpletext Nothing
       return (evt, vp, av, elog)
  renderTemplate "dumpAllEvidenceTexts" $ do
     F.value "lang" $ codeFromLang lang
     F.value "versionID" versionID
     F.value "timestamp" $ show now
     F.objects "evidences" $ for evs $ \(evt, vp, av, elog) -> do
       F.value "name" $ show evt
       F.value "evidencelog" $ elog
       F.value "authorview" $ av
       F.value "verificationpage" $ vp
