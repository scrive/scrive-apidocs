module Doc.Signing.Consumer (
    DocumentSigning
  , documentSigning
  ) where

import Control.Monad.Catch
import Control.Monad.Extra (whenJustM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Crypto.RNG
import Data.Aeson (object)
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
import EID.CGI.GRP.Config
import EID.CGI.GRP.Control
import EID.CGI.GRP.Types as CGI
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Provider.FITupas (FITupasEIDServiceCompletionData(..))
import EID.EIDService.Provider.NLIDIN (NLIDINEIDServiceCompletionData(..))
import EID.EIDService.Provider.NOBankID (NOBankIDEIDServiceCompletionData(..))
import EID.EIDService.Provider.Onfido
  ( CompletionData(..), OnfidoEIDServiceCompletionData(..), completionDataToName
  )
import EID.EIDService.Provider.SEBankID
  ( SEBankIDEIDServiceSignCompletionData(..)
  , SEBankIDEIDServiceSignTransactionData(..), normalisePersonalNumber
  )
import EID.EIDService.Types
import EID.Nets.Config
import EID.Nets.Control (checkNetsSignStatus)
import EID.Nets.Types (NetsSignStatus(..), netsFaultText)
import EID.Signature.Model
import EventStream.Class
import File.FileID
import File.Storage
import File.Types
import GuardTime
import IPAddress
import KontraError
import Log.Identifier
import MailContext
import MinutesTime
import Templates
import Text.JSON.Convert
import User.Lang
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.Flow as Flow
  ( EngineEvent(..), UserAction(Signature), processFlowEventForSignatory
  )
import qualified EID.EIDService.Provider.Verimi as Verimi
import qualified Flow.Model as Flow (selectInstanceIdByDocumentId)
import qualified InputValidation
import qualified MailContext.Internal

data DocumentSigning = DocumentSigning {
    signingSignatoryID          :: SignatoryLinkID
  , signingBrandedDomainID      :: BrandedDomainID
  , signingTime                 :: UTCTime
  , signingClientIP4            :: IPAddress
  , signingClientTime           :: Maybe UTCTime
  , signingClientName           :: Maybe Text
  , signingLang                 :: Lang
  , signingFields               :: SignatoryFieldsValuesForSigning
  , signingAcceptedAttachments  :: [FileID]
  , signingScreenshots          :: SignatoryScreenshots
  , signingLastCheckStatus      :: Maybe Text
  , signingCancelled            :: Bool
  , signingAttempts             :: Int32
  , signingNotUploadedSigAttachments :: [Text]
  , signingSignatureProvider    :: SignatureProvider
  , signingConsentResponses     :: SignatoryConsentResponsesForSigning
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
          let mc = MailContext { lang               = signingLang
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
                  EIDServiceOnfido -> handleEidService
                    (`getTransactionFromEIDService` EIDServiceTransactionProviderOnfido)
                    processCompleteOnfidoTransaction
                    mEidServiceConf
                    ds
                    now
                  EIDServiceNOBankID -> handleEidService
                    (`getTransactionFromEIDService` EIDServiceTransactionProviderNOBankID)
                    processCompleteNOBankIDTransaction
                    mEidServiceConf
                    ds
                    now
                  EIDServiceSEBankID -> handleEidService
                    (checkSEBankID signingSignatoryID)
                    processCompleteSEBankIDTransaction
                    mEidServiceConf
                    ds
                    now
                  EIDServiceVerimi -> handleEidService
                    (`getTransactionFromEIDService` EIDServiceTransactionProviderVerimi)
                    processCompleteVerimiTransaction
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
      let mctd = estRespCompletionData ct
      NLIDINEIDServiceCompletionData {..} <- whenNothing mctd $ throwE (Failed Remove)
      dbUpdate
        . MergeEIDServiceIDINSignature signingSignatoryID
        $ EIDServiceNLIDINSignature { unEIDServiceIDINSigSignatoryName = eiditdName
                                    , unEIDServiceIDINSigDateOfBirth   = eiditdBirthDate
                                    , unEIDServiceIDINSigCustomerID    = eiditdCustomerID
                                    }
      logInfo_ $ "EidHub NL IDIN Sign succeeded: " <> showt est
      signFromESignature ds now
      chargeForItemSingle CIIDINSignatureFinished . documentid =<< theDocument

    checkSEBankID signingSignatoryID conf transactionID = do
      -- We parse data from EID service as a whole, not only the completion data, as we need
      -- process status information from top level entity
      transactionResponse :: Maybe
          (EIDServiceTransactionResponse SEBankIDEIDServiceSignTransactionData) <-
        getTransactionFromEIDService conf
                                     EIDServiceTransactionProviderSEBankID
                                     transactionID

      case transactionResponse of
        Just response -> do
          let statusInfo = estRespCompletionData response >>= eidsebidsProcessStatusInfo
          whenJust statusInfo $ \processStatusInfo -> dbUpdate $ UpdateDocumentSigning
            signingSignatoryID
            (estRespStatus response == EIDServiceTransactionStatusFailed)
            processStatusInfo
          return transactionResponse
        Nothing -> return Nothing

    processCompleteFITupasTransaction ds@DocumentSigning {..} est ct now = do
      let mctd = estRespCompletionData ct
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

      unless personalNumberMatchesSiglink $ do
        dbUpdate $ UpdateDocumentSigning
          signingSignatoryID
          True
          "Personal number of signatory doesn't match the one authenticated by FTN."
        throwE . Ok . RerunAfter $ iminutes minutesTillPurgeOfFailedAction

      dbUpdate $ MergeEIDServiceFITupasSignature signingSignatoryID sig
      logInfo_ . ("EidHub FI TUPAS Sign succeeded: " <>) . showt $ est
      signFromESignature ds now
      chargeForItemSingle CIFITupasSignatureFinished . documentid =<< theDocument

    processCompleteNOBankIDTransaction ds@DocumentSigning {..} est ct now = do
      let mctd = estRespCompletionData ct
      NOBankIDEIDServiceCompletionData {..} <- whenNothing mctd $ throwE (Failed Remove)

      let
        sig = EIDServiceNOBankIDSignature
          { eidServiceNOBankIDSigInternalProvider = eidnobidInternalProvider
          , eidServiceNOBankIDSigSignatoryName    = fromMaybe "" eidnobidName
          , eidServiceNOBankIDSigPhoneNumber      = eidnobidPhoneNumber
          -- make sure we insert empty string, not null for case
          -- where personal number is never provided (mobile bankid)
          , eidServiceNOBankIDSigPersonalNumber   = Just
                                                      $ fromMaybe "" eidnobidPersonalNumber
          , eidServiceNOBankIDSigDateOfBirth      = eidnobidBirthDate
          , eidServiceNOBankIDSigSignedText       = eidnobidSignText
          , eidServiceNOBankIDSigCertificate      = eidnobidCertificate
          }
      dbUpdate $ MergeEIDServiceNOBankIDSignature signingSignatoryID sig
      logInfo_ . ("EidHub NO BankID Sign succeeded: " <>) . showt $ est
      signFromESignature ds now
      chargeForItemSingle CINOBankIDSignatureFinished . documentid =<< theDocument

    processCompleteOnfidoTransaction ds@DocumentSigning {..} est ct now = do
      let mctd = estRespCompletionData ct
      OnfidoEIDServiceCompletionData {..} <- whenNothing mctd $ throwE (Failed Remove)

      let sig = EIDServiceOnfidoSignature
            { eidServiceOnfidoSigSignatoryName = completionDataToName
                                                   eidonfidoCompletionData
            , eidServiceOnfidoSigDateOfBirth   = eidonfidoDateOfBirth
                                                   eidonfidoCompletionData
            , eidServiceOnfidoSigMethod        = eidonfidoMethod
            }

      dbUpdate $ MergeEIDServiceOnfidoSignature signingSignatoryID sig
      logInfo_ . ("EidHub Onfido Sign succeeded: " <>) . showt $ est
      signFromESignature ds now

      let chargeableitem = case eidonfidoMethod of
            OnfidoDocumentCheck         -> CIOnfidoDocumentCheckSignatureFinished
            OnfidoDocumentAndPhotoCheck -> CIOnfidoDocumentAndPhotoCheckSignatureFinished
      chargeForItemSingle chargeableitem . documentid =<< theDocument

    processCompleteSEBankIDTransaction ds@DocumentSigning {..} est ct now = do
      let mctd = estRespCompletionData ct
      SEBankIDEIDServiceSignTransactionData {..} <- whenNothing mctd
        $ throwE (Failed Remove)
      SEBankIDEIDServiceSignCompletionData {..} <- whenNothing eidsebidsCompletionData
        $ throwE (Failed Remove)

      msl <- getSigLinkFor signingSignatoryID <$> theDocument
      let personalNumberMatchesSiglink =
            case normalisePersonalNumber now . getPersonalNumber <$> msl of
              Just InputValidation.Empty -> True
              Just (InputValidation.Good pnFromSigLink) ->
                pnFromSigLink == eidsebidsSignatoryPersonalNumber
              _ -> False

      unless personalNumberMatchesSiglink $ do
        logAttention_
          "Personal number of signatory doesn't match the one authenticated by Swedish BankID."
        dbUpdate $ UpdateDocumentSigning signingSignatoryID
                                         True
                                         (grpFaultText CGI.InternalError)
        throwE . Ok . RerunAfter $ iminutes minutesTillPurgeOfFailedAction

      let sig = EIDServiceSEBankIDSignature
            { eidServiceSEBankIDSigSignatoryName  = eidsebidsSignatoryName
            , eidServiceSEBankIDSigPersonalNumber = eidsebidsSignatoryPersonalNumber
            , eidServiceSEBankIDSigIP             = eidsebidsSignatoryIP
            , eidServiceSEBankIDSigSignedText     = eidsebidsSignedText
            , eidServiceSEBankIDSigSignature      = eidsebidsSignature
            , eidServiceSEBankIDSigOcspResponse   = eidsebidsOcspResponse
            }

      dbUpdate $ MergeEIDServiceSEBankIDSignature signingSignatoryID sig
      logInfo_ . ("EidHub Swedish BankID Sign succeeded: " <>) . showt $ est
      signFromESignature ds now

      chargeForItemSingle CISEBankIDSignatureFinished . documentid =<< theDocument
    processCompleteVerimiTransaction ds@DocumentSigning {..} est ct now = do
      Verimi.VerimiSignCompletionDataFromResponse signCompletionData <-
        whenNothing (estRespCompletionData ct) $ do
          logAttention_
            "Verimi QES failed: missing completion data when processing complete transaction"
          throwE (Failed Remove)

      signature <-
        whenNothing
            (Verimi.eidServiceVerimiQesSignatureFromCompletionData signCompletionData)
          $ do
              logAttention_
                "Verimi QES failed: completion data does not give rise to an EidServiceVerimiQesSignature."
              throwE (Failed Remove)

      signedPdfContent <-
        whenNothing
            (fmap Verimi.documentData . listToMaybe . Verimi.pdfs $ Verimi.signedDocuments
              signCompletionData
            )
          $ do
              logAttention_
                "Verimi QES failed: completion data does not contain a signed pdf!"
              throwE (Failed Remove)

      mainfile <- do
        doc <- theDocument
        whenNothing (documentmainfile doc) $ throwE (Failed Remove)
      signedFile <- saveNewFile (filename mainfile) $ fromBase64 signedPdfContent
      dbUpdate $ AppendPendingVerimiQesFile signedFile
      dbUpdate $ MergeEIDServiceVerimiQesSignature signingSignatoryID signature
      logInfo_ $ "EidHub Verimi QES Sign succeeded: " <> showt est
      signFromESignature ds now
      chargeForItemSingle CIVerimiQesSignatureFinished . documentid =<< theDocument

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
     -> ExceptT Result m (Maybe (EIDServiceTransactionResponse a))
     )
  -> (  DocumentSigning
     -> EIDServiceTransactionFromDB
     -> EIDServiceTransactionResponse a
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
    authorID <- whenNothing (getAuthorUserId doc) $ do
      logAttention "Impossible happened - no author for document"
        $ object [identifier $ documentid doc]
      throwE $ Failed Remove
    ugwp            <- dbQuery . UserGroupGetWithParentsByUserID $ authorID

    eidServiceConf_ <- whenNothing mEidServiceConf $ do
      noConfigurationWarning "EIDService Esigning"
      throwE $ Failed Remove

    let serviceToken = fromMaybe (eidServiceConf_ ^. #eidServiceToken)
                                 (ugwpSettings ugwp ^. #eidServiceToken)
    return $ eidServiceConf_ & #eidServiceToken .~ serviceToken

  est <- do
    mest <- dbQuery
      $ GetEIDServiceTransactionNoSessionIDGuard signingSignatoryID EIDServiceAuthToSign
    whenNothing mest $ throwE (Failed Remove)

  mct <- check conf (estID est)
  ct  <- whenNothing mct $ throwE (Failed Remove)

  let mergeWithStatus newStatus =
        dbUpdate . MergeEIDServiceTransaction $ est { estStatus = newStatus }

  let ts = estRespStatus ct
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
      -- In order for frontend /check calls to work we need to keep failed
      -- signing job around for a few minutes.
      dbUpdate $ UpdateDocumentSigningCancelled signingSignatoryID True
      return . Ok . RerunAfter $ iminutes minutesTillPurgeOfFailedAction

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

  Document { documentid } <- theDocument
  -- If document is part of a flow instance, send relevant event.
  whenJustM (Flow.selectInstanceIdByDocumentId documentid) $ \instanceId -> do
    Flow.processFlowEventForSignatory $ Flow.EngineEvent
      { instanceId   = instanceId
      , userAction   = Flow.Signature
      , signatoryId  = signingSignatoryID
      , documentId   = documentid
      , eventDetails = Nothing
      }
