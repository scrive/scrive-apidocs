module Doc.Signing.Consumer (
    DocumentSigning
  , documentSigning
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.ByteString (ByteString)
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import Text.StringTemplates.Templates (renderTemplate, renderTemplate_)
import qualified Database.Redis as R
import qualified Text.StringTemplates.Fields as F

import AppConf (AmazonConfig)
import BrandedDomain.Model
import DB
import DB.PostgreSQL
import Doc.Action
import Doc.API.V2.Calls.SignatoryCallsUtils
import Doc.API.V2.JSON.Fields
import Doc.Data.AuthorAttachment
import Doc.Data.Document
import Doc.Data.SignatoryLink
import Doc.DocControl
import Doc.DocumentMonad
import Doc.Model.Query
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import Doc.Signing.Model
import EID.CGI.GRP.Config
import EID.CGI.GRP.Control
import EID.CGI.GRP.Data
import EID.Signature.Model
import File.FileID
import GuardTime
import IPAddress
import KontraError
import KontraPrelude
import MailContext
import MailContext.Internal
import MemCache (MemCache)
import MinutesTime
import Templates
import User.Lang
import Util.Actor
import Util.SignatoryLinkUtils
import qualified Amazon as A

data DocumentSigning = DocumentSigning {
    signingSignatoryID          :: !SignatoryLinkID
  , signingBrandedDomainID      :: !BrandedDomainID
  , signingTime                 :: !UTCTime
  , signingClientIP4            :: !IPAddress
  , signingClientTime           :: !(Maybe UTCTime)
  , signingClientName           :: !(Maybe String)
  , signingLang                 :: !Lang
  , signingFields               :: !SignatoryFieldsValuesForSigning
  , signingAcceptedAttachments  :: ![FileID]
  , signingScreenshots          :: !SignatoryScreenshots
  , signingLastCheckStatus      :: !(Maybe String)
  , signingCancelled            :: !Bool
  , signingAttempts             :: !Int32
  , signingNotUploadedSigAttachments :: ![String]
  }

documentSigning
  :: (CryptoRNG m, MonadLog m, MonadIO m, MonadBaseControl IO m, MonadMask m)
  => Maybe AmazonConfig
  -> GuardTimeConf
  -> Maybe CgiGrpConfig
  -> KontrakcjaGlobalTemplates
  -> MemCache FileID ByteString
  -> Maybe R.Connection
  -> ConnectionSourceM m
  -> String
  -> Int
  -> ConsumerConfig m SignatoryLinkID DocumentSigning
documentSigning amazonConf guardTimeConf cgiGrpConf
  templates localCache globalCache pool mailNoreplyAddress maxRunningJobs = ConsumerConfig {
    ccJobsTable = "document_signing_jobs"
  , ccConsumersTable = "document_signing_consumers"
  , ccJobSelectors =
    [ "id"
    , "branded_domain_id"
    , "time"
    , "client_ip_v4"
    , "client_time"
    , "client_name"
    , "lang"
    , "fields"
    , "accepted_attachments"
    , "screenshots"
    , "last_check_status"
    , "cancelled"
    , "attempts"
    , "not_uploaded_sig_attachments"
    ]
  , ccJobFetcher = \(sid, bdid, st, cip, mct, mcn, sl, sf, Array1 saas, ss, mlcs, sc, attempts, Array1 nusa) -> DocumentSigning {
      signingSignatoryID = sid
    , signingBrandedDomainID = bdid
    , signingTime = st
    , signingClientIP4 = cip
    , signingClientTime = mct
    , signingClientName = mcn
    , signingLang = sl
    , signingFields = sf
    , signingAcceptedAttachments = saas
    , signingScreenshots = ss
    , signingLastCheckStatus = mlcs
    , signingCancelled = sc
    , signingAttempts = attempts
    , signingNotUploadedSigAttachments = nusa
    }
  , ccJobIndex = signingSignatoryID
  , ccNotificationChannel = Nothing
  , ccNotificationTimeout = (fromIntegral secondsToRetry) * 1000000
  , ccMaxRunningJobs = maxRunningJobs
  , ccProcessJob = \DocumentSigning{..} ->
      withPostgreSQL pool . withDocumentM (dbQuery $ GetDocumentBySignatoryLinkID signingSignatoryID) $ do
      signingDocumentID <- documentid <$> theDocument
      now <- currentTime
      bd <- dbQuery $ GetBrandedDomainByID signingBrandedDomainID
      let ac = A.AmazonConfig {
              A.awsConfig      = amazonConf
            , A.awsLocalCache  = localCache
            , A.awsGlobalCache = globalCache
            }
          mc = MailContext {
              _mctxlang                 = signingLang
            , _mctxcurrentBrandedDomain = bd
            , _mctxtime                 = now
            , _mctxmailNoreplyAddress   = mailNoreplyAddress
            }
      runTemplatesT (signingLang, templates)
        . A.runAmazonMonadT ac
        . runMailContextT mc
        . runGuardTimeConfT guardTimeConf
        $ if (signingCancelled)
            then if (minutesTillPurgeOfFailedAction `minutesAfter` signingTime > now)
              then return $ Ok $ RerunAfter $ iminutes minutesTillPurgeOfFailedAction
              else return $ Ok Remove
            else do
              logInfo_ "Collecting operation"
              case cgiGrpConf of
                Nothing -> do
                  noConfigurationWarning "CGI Group" -- log a warning rather than raising an error as documentSigning is called from cron
                  return $ Failed Remove
                Just cc -> do
                  collectResult <- checkCGISignStatus cc signingDocumentID signingSignatoryID
                  case collectResult of
                    CGISignStatusAlreadySigned -> return $ Ok Remove
                    CGISignStatusFailed grpFault -> do
                      dbUpdate $ UpdateDocumentSigning signingSignatoryID True (grpFaultText grpFault)
                      return $ Ok $ RerunAfter $ iminutes minutesTillPurgeOfFailedAction
                    CGISignStatusInProgress status -> do
                      dbUpdate $ UpdateDocumentSigning signingSignatoryID False (progressStatusText status)
                      return $ Ok $ RerunAfter $ iseconds secondsToRetry
                    CGISignStatusSuccess ->  do
                      esig <- fromJust <$> dbQuery (GetESignature signingSignatoryID) -- collectRequestForBackroundSigning should return true only if there is ESignature in DB
                      initialDoc <- theDocument
                      let sl = fromJust (getSigLinkFor signingSignatoryID initialDoc)
                          magicHash = signatorymagichash sl
                      initialActor <- recreatedSignatoryActor signingTime signingClientTime signingClientName signingClientIP4 sl
                      fieldsWithFiles <- fieldsToFieldsWithFiles signingFields

                      dbUpdate $ UpdateFieldsForSigning sl (fst fieldsWithFiles) (snd fieldsWithFiles) initialActor

                      slWithUpdatedName <- fromJust <$> getSigLinkFor signingSignatoryID <$> theDocument
                      actorWithUpdatedName <- recreatedSignatoryActor signingTime signingClientTime signingClientName signingClientIP4 slWithUpdatedName

                      authorAttachmetsWithAcceptanceText <- forM (documentauthorattachments initialDoc) $ \a -> do
                        acceptanceText <- renderTemplate "_authorAttachmentsUnderstoodContent" (F.value "attachment_name" $ authorattachmentname a)
                        return (acceptanceText,a)
                      dbUpdate $ AddAcceptedAuthorAttachmentsEvents slWithUpdatedName signingAcceptedAttachments authorAttachmetsWithAcceptanceText actorWithUpdatedName

                      notUploadedSigAttachmentsText <- renderTemplate_ "_pageDocumentForAuthorHelpersLocalDialogsAttachmentmarkasnotuploaded"
                      let notUploadedSigAttachmentsWithText = zip signingNotUploadedSigAttachments (repeat notUploadedSigAttachmentsText)
                      dbUpdate $ AddNotUploadedSignatoryAttachmentsEvents sl notUploadedSigAttachmentsWithText actorWithUpdatedName

                      actorWithUpdatedNameAndCurrentTime <- recreatedSignatoryActor now signingClientTime signingClientName signingClientIP4 slWithUpdatedName
                      dbUpdate $ SignDocument signingSignatoryID magicHash (Just esig) Nothing signingScreenshots actorWithUpdatedNameAndCurrentTime

                      postDocumentPendingChange initialDoc
                      handleAfterSigning signingSignatoryID
                      return $ Ok Remove
  , ccOnException = \_ DocumentSigning{..} -> do
      now <- currentTime
      if (minutesTillPurgeOfFailedAction `minutesAfter` signingTime > now)
        then return $ RerunAfter $ iseconds secondsToRetry
        else return Remove
  }
  where
    minutesTillPurgeOfFailedAction :: Int32
    minutesTillPurgeOfFailedAction = 3
    secondsToRetry :: Int32
    secondsToRetry = 5
