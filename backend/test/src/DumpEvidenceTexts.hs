module DumpEvidenceTexts (dumpAllEvidenceTexts) where

import Control.Exception (evaluate)
import Control.Monad.Catch
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Data.Decimal (realFracToDecimal)
import Data.Function (on)
import Log
import System.FilePath ((</>))
import Test.Framework (Test)
import Text.StringTemplates.Templates (TemplatesMonad, renderTemplate)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import DB (MonadDB)
import Doc.DocStateData
import Doc.DocumentMonad (theDocument, withDocument, withDocumentID)
import Doc.SignatoryIdentification (signatoryIdentifierMap)
import Doc.SignatoryLinkID (unsafeSignatoryLinkID)
import EvidenceLog.Model
  ( CurrentEvidenceEventType(..), DocumentEvidenceEvent(..)
  , EvidenceEventType(..), evidenceLogText )

import EvidenceLog.View (historyEventType, simplifiedEventText)
import EvidencePackage.EvidenceLog (finalizeEvidenceText)
import MinutesTime
import Templates (runTemplatesT)
import TestingUtil
  ( addNewRandomUser, addRandomDocumentWithAuthor, fieldForTests, testThat )

import TestKontra (TestEnvSt, teGlobalTemplates, teOutputDirectory)
import Text.XML.DirtyContent (renderXMLContent)
import User.Model (Lang, allLangs, codeFromLang)
import Util.Actor (Actor(..), actorAPIString, actorEmail, actorIP, actorUserID)
import Util.SignatoryLinkUtils (getAuthorSigLink)
import VersionTH (versionID)

dumpAllEvidenceTexts :: TestEnvSt -> Test
dumpAllEvidenceTexts env = testThat "Generating all evidence texts" env $ do
  author <- addNewRandomUser
  did <- addRandomDocumentWithAuthor author
  withDocumentID did $ forM_ allLangs $ \lang -> do
    gts <- asks (get teGlobalTemplates)
    now <- currentTime
    t <- runTemplatesT (lang, gts) $ theDocument >>= dumpEvidenceTexts now lang
    case (get teOutputDirectory env) of
      Just d  -> liftIO $ writeFile
                 (d </> "evidence-texts-" <> (T.unpack $ codeFromLang lang) <> ".html") t
      Nothing -> liftIO $ evaluate t >> return ()

dumpEvidenceTexts :: ( MonadDB m, MonadLog m, MonadThrow m, TemplatesMonad m)
                  => UTCTime -> Lang -> Document -> m String
dumpEvidenceTexts now lang doc' = do
  let Just author_sl' = getAuthorSigLink doc'
      author_sl = author_sl'
            { signatoryfields = [
                  fieldForTests (NameFI $ NameOrder 1) "Adam"
                , fieldForTests (NameFI $ NameOrder 2) "Author"
                , fieldForTests EmailFI "author@example.com"
               ]
            , signatoryrole   = SignatoryRoleSigningParty
            , signatorylinkid = unsafeSignatoryLinkID 1
            }
  let Just time = parseTime' "%d-%m-%Y" "01-01-2013"
  let actor = Actor { actorTime = time
                    , actorClientTime = Nothing
                    , actorClientName = Nothing
                    , actorIP = Nothing
                    , actorUserID = Nothing
                    , actorEmail = Just "author@example.com"
                    , actorSigLinkID = Just (signatorylinkid author_sl)
                    , actorAPIString = Nothing
                    , actorWho = "the author (" <>
                                 fromJust (actorEmail actor) <> ")"
                    }
  let evidencetypes = ([minBound .. maxBound] :: [CurrentEvidenceEventType])
  let asl = defaultSignatoryLink
            {   signatoryfields = [
                      fieldForTests (NameFI $ NameOrder 1) "Sven"
                    , fieldForTests (NameFI $ NameOrder 2) "Signatory"
                    , fieldForTests EmailFI "signatory@example.com"
                ]
            , signatoryrole = SignatoryRoleSigningParty
            , signatorylinkdeliverymethod = EmailAndMobileDelivery
            , signatorylinkid = unsafeSignatoryLinkID 2
            }
  let doc = doc' { documentsignatorylinks = [author_sl, asl] }
  let messageText = Just "This is a <b>custom message</b>"
  let fields t = do
        when (t `elem` [ AutomaticReminderSent
                       , ReminderSend
                       , DeleteSigAttachmentEvidence
                       , SaveSigAttachmentEvidence]) $ do
          F.value "author" $ actorEmail actor
        F.value "description" ("This is a description." :: String)
        F.value "lang" $ show lang
        F.value "msg"
          ("Really long custom message from external eID system." :: String)
        F.value "name" ("some name" :: String)
        F.value "newemail" ("good@example.com" :: String)
        F.value "newphone" ("good-12 34 56" :: String)
        F.value "oldemail" ("bad@example.com" :: String)
        F.value "oldphone" ("bad-12 34 56" :: String)
        F.value "timeouttime" $ formatTimeUTC time
        F.value "timezone" ("Europe/Stockholm" :: String)
        F.value "value" ("field value" :: String)
        F.value "fieldname" ("field name" :: String)
        F.objects "placements" $
          for [(1::Int,0.123::Double,0.42::Double)] $
          \(page,x,y) -> do
            F.value "page" $ page
            F.value "x" $ show $ realFracToDecimal 3 $ x
            F.value "y" $ show $ realFracToDecimal 3 $ y
  let mkev text msgtext amsgtext evt =
          DocumentEvidenceEvent
          { evDocumentID            = documentid doc
          , evTime                  = time
          , evClientTime            = Nothing
          , evClientName            = Nothing
          , evClockErrorEstimate    = Nothing
          , evText                  = text
          , evType                  = Current evt
          , evVersionID             = T.pack versionID
          , evEmail                 = actorEmail actor
          , evUserID                = actorUserID actor
          , evIP4                   = actorIP actor
          , evSigLink               = actorSigLinkID actor
          , evAPI                   = actorAPIString actor
          , evAffectedSigLink       = Just (signatorylinkid asl)
          , evActor                 = actorWho actor
          , evMessageText           = msgtext
          , evAdditionalMessageText = amsgtext
          }
  evs <- (sortBy (compare `on` (\(evt, _, _) -> show evt)) <$>) $
         forM (evidencetypes) $ \evt ->
    do let (msgtext,amsgtext) =
             if evt `elem` [ SMSPinSendEvidence
                           , SMSPinDeliveredEvidence ]
             then (Just "+481234567890", Nothing)
             else
               if evt `elem` [AuthorAttachmentAccepted]
               then ( Just "Attachment name"
                    , Just "Do you accept attachmnet Attachment name")
               else (messageText,Nothing)

       elog <- withDocument doc $
         evidenceLogText evt (fields evt) (Just asl) msgtext amsgtext

       let ev  = mkev elog msgtext amsgtext evt
           sim = signatoryIdentifierMap True [doc]
                 (Set.fromList  [signatorylinkid asl])
           simpletext = if historyEventType (Current evt)
                        then Just <$> simplifiedEventText Nothing sim ev
                        else return Nothing
       av <- if (historyEventType $ Current evt)
             then simpletext
             else return Nothing
       return (evt, av, finalizeEvidenceText sim ev "Not named party")

  renderTemplate "dumpAllEvidenceTexts" $ do
     F.value "lang" $ codeFromLang lang
     F.value "versionID" versionID
     F.value "timestamp" $ show now
     F.objects "evidences" $ for evs $ \(evt, av, elog) -> do
       F.value "unavailable" $ evt `elem` [AuthenticatedToViewEvidence]
       F.value "name" $ show evt
       F.value "evidencelog" $ renderXMLContent elog
       F.value "authorview" $ av
