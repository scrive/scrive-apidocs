module Doc.Signing.Consumer (
    DocumentSigning
  , documentSigning
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Crypto.RNG
import Data.Aeson ((.=), object)
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.Text as T
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
import Util.HasSomeUserInfo
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

minutesTillPurgeOfFailedAction :: Int32
minutesTillPurgeOfFailedAction = 3

secondsToRetry :: Int32
secondsToRetry = 5

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
    , ccNotificationTimeout = fromIntegral secondsToRetry * 1000000
    , ccMaxRunningJobs      = maxRunningJobs
    , ccProcessJob          =
      \ds@DocumentSigning {..} -> withPostgreSQL pool $ do
        let getDocM = dbQuery $ GetDocumentBySignatoryLinkID signingSignatoryID
        withDocumentM getDocM $ do
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
            $ if signingCancelled
                then if minutesTillPurgeOfFailedAction `minutesAfter` signingTime > now
                  then return . Ok . RerunAfter $ iminutes minutesTillPurgeOfFailedAction
                  else return $ Ok Remove
                else runHandler $ case signingSignatureProvider of
                  CgiGrpBankID   -> handleCgiGrpBankID cgiGrpConf ds now
                  NetsNOBankID   -> handleNets netsSignConf ds now
                  NetsDKNemID    -> handleNets netsSignConf ds now
                  EIDServiceIDIN -> handleEidService
                    (`getTransactionFromEIDService` EIDServiceTransactionProviderNLIDIN)
                    processCompleteIDINTransaction
                    mEidServiceConf
                    ds
                    now
                  EIDServiceTupas -> handleEidService
                    (`getTransactionFromEIDService` EIDServiceTransactionProviderFITupas)
                    processCompleteFITupasTransaction
                    mEidServiceConf
                    ds
                    now
                  LegacyBankID -> legacyProviderFail signingSignatoryID LegacyBankID
                  LegacyTelia  -> legacyProviderFail signingSignatoryID LegacyTelia
                  LegacyNordea -> legacyProviderFail signingSignatoryID LegacyNordea
                  LegacyMobileBankID ->
                    legacyProviderFail signingSignatoryID LegacyMobileBankID
    , ccOnException         =
      \_ DocumentSigning {..} -> do
        now <- currentTime
        if minutesTillPurgeOfFailedAction `minutesAfter` signingTime > now
          then return . RerunAfter $ iseconds secondsToRetry
          else return Remove
    }
  where
    -- By wrapping our handlers in ExceptT Result _ Result we are able to encode
    -- 'early exits' much more ergonomically (using `throwE`).
    runHandler :: Monad m => ExceptT Result m Result -> m Result
    runHandler handler = either identity identity <$> runExceptT handler

    legacyProviderFail signingSignatoryID provider = do
      logAttention "Legacy provider used in signing consumer"
        $ object [identifier signingSignatoryID, "provider" .= showt provider]
      throwE $ Failed Remove

    processCompleteIDINTransaction ds@DocumentSigning {..} est ct now = do
      let mctd = case ct of
            CompleteNLIDINEIDServiceTransaction _ mctd' -> mctd'
            _ -> Nothing
      ctd <- whenNothing mctd $ throwE (Failed Remove)
      dbUpdate
        . MergeEIDServiceIDINSignature signingSignatoryID
        $ EIDServiceNLIDINSignature ctd
      logInfo_ . ("EidHub NL IDIN Sign succeeded: " <>) . showt $ est
      signFromESignature ds now
      chargeForItemSingle CIIDINSignature . documentid =<< theDocument

    processCompleteFITupasTransaction ds@DocumentSigning {..} est ct now = do
      let mctd = case ct of
            CompleteFITupasEIDServiceTransaction _ mctd' -> mctd'
            _ -> Nothing
      FITupasEIDServiceCompletionData {..} <- whenNothing mctd $ throwE (Failed Remove)
      let sig = EIDServiceFITupasSignature
            { eidServiceFITupasSigSignatoryName  = eidtupasName
            , eidServiceFITupasSigPersonalNumber = eidtupasSSN
            , eidServiceFITupasSigDateOfBirth    = eidtupasBirthDate
            }

      msl <- getSigLinkFor signingSignatoryID <$> theDocument
      let personalNumberMatchesSiglink
            | Just sl <- msl
            = T.null (getPersonalNumber sl)
              || isNothing eidtupasSSN -- 'legal persons' don't always have an SSN
              || (getPersonalNumber sl == fromMaybe "" eidtupasSSN)
            | otherwise
            = False
      unless personalNumberMatchesSiglink $ throwE (Failed Remove)

      dbUpdate $ MergeEIDServiceFITupasSignature signingSignatoryID sig
      logInfo_ . ("EidHub FI TUPAS Sign succeeded: " <>) . showt $ est
      signFromESignature ds now
      chargeForItemSingle CIFITupasSignature . documentid =<< theDocument


handleCgiGrpBankID
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadEventStream m
     , DocumentMonad m
     , TemplatesMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     )
  => Maybe CgiGrpConfig
  -> DocumentSigning
  -> UTCTime
  -> ExceptT Result m Result
handleCgiGrpBankID mCgiGrpConf ds@DocumentSigning {..} now = do
  logInfo_ "Collecting operation for CGI signing"

  conf <- whenNothing mCgiGrpConf $ do
    noConfigurationWarning "CGI Group"  -- log a warning rather than raising an error as documentSigning is called from cron
    throwE $ Failed Remove

  signingDocumentID <- documentid <$> theDocument
  collectResult     <- checkCGISignStatus conf signingDocumentID signingSignatoryID
  logInfo "CGI collect result" $ object ["result" .= show collectResult]
  case collectResult of
    CGISignStatusAlreadySigned   -> return $ Ok Remove
    CGISignStatusFailed grpFault -> do
      dbUpdate $ UpdateDocumentSigning signingSignatoryID True (grpFaultText grpFault)
      return . Ok . RerunAfter $ iminutes minutesTillPurgeOfFailedAction
    CGISignStatusInProgress status -> do
      dbUpdate
        $ UpdateDocumentSigning signingSignatoryID False (progressStatusText status)
      return . Ok . RerunAfter $ iseconds secondsToRetry
    CGISignStatusSuccess -> do
      signFromESignature ds now
      return $ Ok Remove


handleNets
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadEventStream m
     , DocumentMonad m
     , TemplatesMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     )
  => Maybe NetsSignConfig
  -> DocumentSigning
  -> UTCTime
  -> ExceptT Result m Result
handleNets mNetsSignConf ds@DocumentSigning {..} now = do
  logInfo_ "Collecting operation for Nets signing"

  conf@NetsSignConfig {..} <- whenNothing mNetsSignConf $ do
    noConfigurationWarning "Nets Esigning" -- log a warning rather than raising an error as documentSigning is called from cron
    throwE $ Failed Remove

  logInfo_ "NETS Signing Executed!"
  signingDocumentID <- documentid <$> theDocument
  signResult        <- checkNetsSignStatus conf signingDocumentID signingSignatoryID
  case signResult of
    NetsSignStatusSuccess -> do
      signFromESignature ds now
      return $ Ok Remove
    NetsSignStatusFailure fault -> do
      dbUpdate $ UpdateDocumentSigning signingSignatoryID True (netsFaultText fault)
      return . Ok . RerunAfter $ iminutes minutesTillPurgeOfFailedAction
    NetsSignStatusInProgress -> do
      dbUpdate $ UpdateDocumentSigning signingSignatoryID False "nets_in_progress"
      return . Ok . RerunAfter $ iseconds secondsToRetry
    NetsSignStatusAlreadySigned -> return $ Ok Remove


handleEidService
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadEventStream m
     , DocumentMonad m
     , TemplatesMonad m
     )
  => (  EIDServiceConf
     -> EIDServiceTransactionID
     -> ExceptT Result m (Maybe CompleteEIDServiceTransaction)
     )
  -> (  DocumentSigning
     -> EIDServiceTransactionFromDB
     -> CompleteEIDServiceTransaction
     -> UTCTime
     -> ExceptT Result m ()
     )
  -> Maybe EIDServiceConf
  -> DocumentSigning
  -> UTCTime
  -> ExceptT Result m Result
handleEidService check process mEidServiceConf ds@DocumentSigning {..} now = do
  doc  <- theDocument

  conf <- do
    authorID <- whenNothing (maybesignatory =<< getAuthorSigLink doc) $ do
      logAttention "Impossible happened - no author for document"
        $ object [identifier $ documentid doc]
      throwE $ Failed Remove
    ugwp           <- dbQuery . UserGroupGetWithParentsByUserID $ authorID

    eidServiceConf <- whenNothing mEidServiceConf $ do
      noConfigurationWarning "EIDService Esigning"
      throwE $ Failed Remove

    let serviceToken = fromMaybe (eidServiceConf ^. #eidServiceToken)
                                 (ugwpSettings ugwp ^. #eidServiceToken)
    return $ eidServiceConf & #eidServiceToken .~ serviceToken

  est <- do
    mest <- dbQuery
      $ GetEIDServiceTransactionNoSessionIDGuard signingSignatoryID EIDServiceAuthToSign
    whenNothing mest $ throwE (Failed Remove)

  mct <- check conf (estID est)
  ct  <- whenNothing mct $ throwE (Failed Remove)

  let mergeWithStatus newStatus =
        dbUpdate . MergeEIDServiceTransaction $ est { estStatus = newStatus }

  let ts = transactionStatus ct
  case ts of
    EIDServiceTransactionStatusCompleteAndSuccess -> do
      process ds est ct now
      mergeWithStatus EIDServiceTransactionStatusCompleteAndSuccess
      return $ Ok Remove

    EIDServiceTransactionStatusNew -> do
      mergeWithStatus ts
      return . Ok . RerunAfter $ iseconds secondsToRetry

    EIDServiceTransactionStatusStarted -> do
      mergeWithStatus ts
      return . Ok . RerunAfter $ iseconds secondsToRetry

    -- EIDServiceTransactionStatusFailed
    -- EIDServiceTransactionStatusCompleteAndFailed
    _ -> do
      mergeWithStatus ts
      throwE $ Failed Remove


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

  dbUpdate $ uncurry (UpdateFieldsForSigning sl) fieldsWithFiles initialActor

  slWithUpdatedName    <- fromJust . getSigLinkFor signingSignatoryID <$> theDocument
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
