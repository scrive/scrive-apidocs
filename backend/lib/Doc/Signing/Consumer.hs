module Doc.Signing.Consumer (
    DocumentSigning
  , documentSigning
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson ((.=), object)
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.Model
import Chargeable
import DB
import DB.PostgreSQL
import Doc.Action
import Doc.API.V2.Calls.SignatoryCallsUtils
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.DocControl
import Doc.DocumentMonad
import Doc.Model.Query
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import Doc.Signing.Model
import Doc.Types.AuthorAttachment
import Doc.Types.Document
import Doc.Types.SignatoryLink
import EID.CGI.GRP.Config
import EID.CGI.GRP.Control
import EID.CGI.GRP.Types
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import EID.Nets.Config
import EID.Nets.Control (checkNetsSignStatus)
import EID.Nets.Types (NetsSignStatus(..), netsFaultText)
import EID.Signature.Model
import EventStream.Class
import File.FileID
import FileStorage
import GuardTime
import IPAddress
import KontraError
import Log.Identifier
import MailContext
import MinutesTime
import Templates
import User.Lang
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.SignatoryLinkUtils
import qualified MailContext.Internal as I

data DocumentSigning = DocumentSigning {
    signingSignatoryID          :: !SignatoryLinkID
  , signingBrandedDomainID      :: !BrandedDomainID
  , signingTime                 :: !UTCTime
  , signingClientIP4            :: !IPAddress
  , signingClientTime           :: !(Maybe UTCTime)
  , signingClientName           :: !(Maybe Text)
  , signingLang                 :: !Lang
  , signingFields               :: !SignatoryFieldsValuesForSigning
  , signingAcceptedAttachments  :: ![FileID]
  , signingScreenshots          :: !SignatoryScreenshots
  , signingLastCheckStatus      :: !(Maybe Text)
  , signingCancelled            :: !Bool
  , signingAttempts             :: !Int32
  , signingNotUploadedSigAttachments :: ![Text]
  , signingSignatureProvider    :: !SignatureProvider
  , signingConsentResponses     :: !SignatoryConsentResponsesForSigning
  }

documentSigning
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadEventStream m
     )
  => GuardTimeConf
  -> Maybe CgiGrpConfig
  -> Maybe NetsSignConfig
  -> Maybe EIDServiceConf
  -> KontrakcjaGlobalTemplates
  -> ConnectionSourceM m
  -> Text
  -> Int
  -> ConsumerConfig m SignatoryLinkID DocumentSigning
documentSigning guardTimeConf cgiGrpConf netsSignConf mEidServiceConf templates pool mailNoreplyAddress maxRunningJobs
  = ConsumerConfig
    { ccJobsTable           = "document_signing_jobs"
    , ccConsumersTable      = "document_signing_consumers"
    , ccJobSelectors        = [ "id"
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
                              , "signature_provider"
                              , "consent_responses"
                              ]
    , ccJobFetcher          =
      \(sid, bdid, st, cip, mct, mcn, sl, sf, Array1 saas, ss, mlcs, sc, attempts, Array1 nusa, sp, crs) ->
        DocumentSigning { signingSignatoryID         = sid
                        , signingBrandedDomainID     = bdid
                        , signingTime                = st
                        , signingClientIP4           = cip
                        , signingClientTime          = mct
                        , signingClientName          = mcn
                        , signingLang                = sl
                        , signingFields              = sf
                        , signingAcceptedAttachments = saas
                        , signingScreenshots         = ss
                        , signingLastCheckStatus     = mlcs
                        , signingCancelled           = sc
                        , signingAttempts            = attempts
                        , signingNotUploadedSigAttachments = nusa
                        , signingSignatureProvider   = sp
                        , signingConsentResponses    = crs
                        }
    , ccJobIndex            = signingSignatoryID
    , ccNotificationChannel = Nothing
    , ccNotificationTimeout = (fromIntegral secondsToRetry) * 1000000
    , ccMaxRunningJobs      = maxRunningJobs
    , ccProcessJob          =
      \ds@DocumentSigning {..} ->
        withPostgreSQL pool
          . withDocumentM (dbQuery $ GetDocumentBySignatoryLinkID signingSignatoryID)
          $ do
              now <- currentTime
              bd  <- dbQuery $ GetBrandedDomainByID signingBrandedDomainID
              let mc = I.MailContext { lang               = signingLang
                                     , brandedDomain      = bd
                                     , time               = now
                                     , mailNoreplyAddress = mailNoreplyAddress
                                     }
              runTemplatesT (signingLang, templates)
                . runMailContextT mc
                . runGuardTimeConfT guardTimeConf
                $ if (signingCancelled)
                    then
                      if (minutesTillPurgeOfFailedAction `minutesAfter` signingTime > now)
                        then return $ Ok $ RerunAfter $ iminutes
                          minutesTillPurgeOfFailedAction
                        else return $ Ok Remove
                    else case signingSignatureProvider of
                      CgiGrpBankID   -> handleCgiGrpBankID ds now
                      NetsNOBankID   -> handleNets ds now
                      NetsDKNemID    -> handleNets ds now
                      EIDServiceIDIN -> handleEidService ds now
                      LegacyBankID   -> legacyProviderFail signingSignatoryID LegacyBankID
                      LegacyTelia    -> legacyProviderFail signingSignatoryID LegacyTelia
                      LegacyNordea   -> legacyProviderFail signingSignatoryID LegacyNordea
                      LegacyMobileBankID ->
                        legacyProviderFail signingSignatoryID LegacyMobileBankID
    , ccOnException         =
      \_ DocumentSigning {..} -> do
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

    legacyProviderFail signingSignatoryID provider = do
      logAttention "Legacy provider used in signing consumer"
        $ object [identifier signingSignatoryID, "provider" .= showt provider]
      return $ Failed Remove

    handleCgiGrpBankID ds@DocumentSigning {..} now = do
      logInfo_ "Collecting operation for CGI signing"
      case cgiGrpConf of
        Nothing -> do
          noConfigurationWarning "CGI Group" -- log a warning rather than raising an error as documentSigning is called from cron
          return $ Failed Remove
        Just cc -> do
          signingDocumentID <- documentid <$> theDocument
          collectResult     <- checkCGISignStatus cc signingDocumentID signingSignatoryID
          logInfo "CGI collect result" $ object ["result" .= show collectResult]
          case collectResult of
            CGISignStatusAlreadySigned   -> return $ Ok Remove
            CGISignStatusFailed grpFault -> do
              dbUpdate
                $ UpdateDocumentSigning signingSignatoryID True (grpFaultText grpFault)
              return $ Ok $ RerunAfter $ iminutes minutesTillPurgeOfFailedAction
            CGISignStatusInProgress status -> do
              dbUpdate $ UpdateDocumentSigning signingSignatoryID
                                               False
                                               (progressStatusText status)
              return $ Ok $ RerunAfter $ iseconds secondsToRetry
            CGISignStatusSuccess -> do
              signFromESignature ds now
              return $ Ok Remove
    handleNets ds@DocumentSigning {..} now = do
      logInfo_ "Collecting operation for Nets signing"
      case netsSignConf of
        Nothing -> do
          noConfigurationWarning "Nets Esigning" -- log a warning rather than raising an error as documentSigning is called from cron
          return $ Failed Remove
        Just conf@NetsSignConfig {..} -> do
          logInfo_ "NETS Signing Executed!"
          signingDocumentID <- documentid <$> theDocument
          signResult <- checkNetsSignStatus conf signingDocumentID signingSignatoryID
          case signResult of
            NetsSignStatusSuccess -> do
              signFromESignature ds now
              return $ Ok Remove
            NetsSignStatusFailure fault -> do
              dbUpdate
                $ UpdateDocumentSigning signingSignatoryID True (netsFaultText fault)
              return $ Ok $ RerunAfter $ iminutes minutesTillPurgeOfFailedAction
            NetsSignStatusInProgress -> do
              dbUpdate $ UpdateDocumentSigning signingSignatoryID False "nets_in_progress"
              return $ Ok $ RerunAfter $ iseconds secondsToRetry
            NetsSignStatusAlreadySigned -> return $ Ok Remove
    handleEidService ds@DocumentSigning {..} now = do
      case mEidServiceConf of
        Nothing -> do
          noConfigurationWarning "EIDService Esigning"
          return $ Failed Remove
        Just conf0 -> do
          doc <- dbQuery $ GetDocumentBySignatoryLinkID signingSignatoryID
          case maybesignatory =<< getAuthorSigLink doc of
            Nothing -> do
              logAttention "Impossible happened - no author for document"
                $ object [identifier $ documentid doc]
              return $ Failed Remove
            Just authorID -> do
              ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ authorID
              let conf = case ugwpSettings ugwp ^. #eidServiceToken of
                    Nothing    -> conf0
                    Just token -> set #eidServiceToken token conf0
              mest <- dbQuery $ GetEIDServiceTransactionNoSessionIDGuard
                signingSignatoryID
                EIDServiceAuthToSign
              res <- case mest of
                Nothing  -> return Nothing
                Just est -> checkIDINTransactionWithEIDService conf (estID est) >>= \case
                  (Nothing, _   ) -> return Nothing
                  (Just ts, mctd) -> return $ Just (est, ts, mctd)
              case res of
                Just (est, ts, mctd) -> do
                  let mergeWithStatus newStatus =
                        dbUpdate $ MergeEIDServiceTransaction $ est
                          { estStatus = newStatus
                          }
                  case (ts, mctd) of
                    (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> do
                      dbUpdate
                        . MergeEIDServiceIDINSignature signingSignatoryID
                        $ EIDServiceIDINSignature cd
                      logInfo_
                        . ("EidHub NL IDIN Sign succeeded: " <>)
                        . showt
                        $ (est, ts)
                      signFromESignature ds now
                      chargeForItemSingle CIIDINSignature $ documentid doc
                      mergeWithStatus EIDServiceTransactionStatusCompleteAndSuccess
                      return $ Ok Remove
                    (EIDServiceTransactionStatusCompleteAndSuccess, Nothing) -> do
                      mergeWithStatus EIDServiceTransactionStatusCompleteAndFailed
                      return $ Failed Remove
                    (EIDServiceTransactionStatusNew, _) -> do
                      mergeWithStatus ts
                      return $ Ok $ RerunAfter $ iseconds secondsToRetry
                    (EIDServiceTransactionStatusStarted, _) -> do
                      mergeWithStatus ts
                      return $ Ok $ RerunAfter $ iseconds secondsToRetry
                    _ -> do
                      mergeWithStatus ts
                      return $ Failed Remove
                _ -> do
                  return $ Failed Remove

signFromESignature
  :: ( GuardTimeConfMonad m
     , MailContextMonad m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , CryptoRNG m
     , DocumentMonad m
     , TemplatesMonad m
     , MonadEventStream m
     )
  => DocumentSigning
  -> UTCTime
  -> m ()
signFromESignature DocumentSigning {..} now = do
  esig       <- fromJust <$> dbQuery (GetESignature signingSignatoryID) -- checkNetsSignStatus should return true only if there is ESignature in DB
  initialDoc <- theDocument
  let sl = fromJust (getSigLinkFor signingSignatoryID initialDoc)
  initialActor <- recreatedSignatoryActor signingTime
                                          signingClientTime
                                          signingClientName
                                          signingClientIP4
                                          sl
  fieldsWithFiles <- fieldsToFieldsWithFiles signingFields

  dbUpdate
    $ UpdateFieldsForSigning sl (fst fieldsWithFiles) (snd fieldsWithFiles) initialActor

  slWithUpdatedName    <- fromJust <$> getSigLinkFor signingSignatoryID <$> theDocument
  actorWithUpdatedName <- recreatedSignatoryActor signingTime
                                                  signingClientTime
                                                  signingClientName
                                                  signingClientIP4
                                                  slWithUpdatedName

  dbUpdate
    $ UpdateConsentResponsesForSigning sl signingConsentResponses actorWithUpdatedName

  authorAttachmetsWithAcceptanceText <-
    forM (documentauthorattachments initialDoc) $ \a -> do
      acceptanceText <- renderTextTemplate
        "_authorAttachmentsUnderstoodContent"
        (F.value "attachment_name" $ authorattachmentname a)
      return (acceptanceText, a)
  dbUpdate $ AddAcceptedAuthorAttachmentsEvents slWithUpdatedName
                                                signingAcceptedAttachments
                                                authorAttachmetsWithAcceptanceText
                                                actorWithUpdatedName

  notUploadedSigAttachmentsText <- renderTextTemplate_
    "_pageDocumentForAuthorHelpersLocalDialogsAttachmentmarkasnotuploaded"
  let notUploadedSigAttachmentsWithText =
        zip signingNotUploadedSigAttachments (repeat notUploadedSigAttachmentsText)
  dbUpdate $ AddNotUploadedSignatoryAttachmentsEvents sl
                                                      notUploadedSigAttachmentsWithText
                                                      actorWithUpdatedName

  actorWithUpdatedNameAndCurrentTime <- recreatedSignatoryActor now
                                                                signingClientTime
                                                                signingClientName
                                                                signingClientIP4
                                                                slWithUpdatedName
  dbUpdate $ SignDocument signingSignatoryID
                          (Just esig)
                          Nothing
                          signingScreenshots
                          actorWithUpdatedNameAndCurrentTime

  postDocumentPendingChange initialDoc sl
  handleAfterSigning signingSignatoryID
