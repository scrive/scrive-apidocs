module EID.Nets.Control (
    netsRoutes
  , checkNetsSignStatus
  , NetsSignStatus(..)) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson ((.=), object)
import Data.String.Utils (replace)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.StringTemplates.Fields as F

import AppView
import Chargeable.Model
import DB
import Doc.DocInfo (isPending)
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import Doc.SignatoryLinkID
import EID.Nets.Call
import EID.Nets.Config
import EID.Nets.Data
import EID.Nets.Model
import EID.Nets.SignID
import EvidenceLog.Model
import FlashMessage
import Happstack.Fields
import InternalResponse
import Kontra hiding (InternalError)
import KontraLink
import KontraPrelude
import Log.Identifier
import MinutesTime
import Network.SOAP.Call
import Network.SOAP.Transport.Curl (curlTransport)
import Network.XMLCurl (CurlAuth(..), SSL(..), mkCertErrorHandler, mkDebugFunction)
import Routing
import Session.Data
import Session.Model
import Templates
import User.Lang
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.HTTP
import qualified EID.Authentication.Model as EID
import qualified EID.Signature.Model as ESign

netsRoutes :: Route (Kontra Response)
netsRoutes = choice [
      dir "resolve" . hGet  . toK0 $ handleResolve
    -- Bellow are error pages - names are based on nets parameters, not what they are
    , dir "status" . hGet  . toK0 $ handleNetsError
    , dir "start"  . hGet  . toK0 $ handleNetsError
    -- Nets esigning pages
    , dir "sign"       . hPost . toK2 $ handleSignRequest
    , dir "sign_error" . hGet  . toK0 $ handleSignError
    , dir "sign_exit"  . hGet  . toK0 $ handleSignExit
    , dir "sign_abort"  . hGet  . toK0 $ handleSignAbort
  ]

----------------------------------------

formatDOB :: T.Text -> T.Text
formatDOB s = case T.splitOn "." s of
               [day, month, year] -> day <> month <> (T.drop 2 year)
               _ -> $unexpectedError "Nets returned date of birth in invalid format"

dobFromDKPersonalNumber :: String -> T.Text
dobFromDKPersonalNumber personalnumber = case T.chunksOf 2 (T.pack $ take 6 $ personalnumber) of
  [day, month, year] -> day <> "." <> month <> "." <> year
  _ -> $unexpectedError $ "This personal number cannot be formatted to date: " <> personalnumber

cnFromDN ::T.Text -> T.Text
cnFromDN dn =
  fromMaybe parseError $ lookup "CN" $ fmap parsePair $ concatMap (T.splitOn " + ") $ T.splitOn ", " $ dn
    where
      parsePair s = case T.splitOn "=" s of
        (name:values) -> (name, T.intercalate "=" values)
        _ -> $unexpectedError $ "Cannot parse DN value: " <> show dn
      parseError = $unexpectedError $ "Cannot parse DN value: " <> show dn

decodeCertificate :: T.Text -> B.ByteString
decodeCertificate = either ($unexpectedError $ "invalid base64 of nets certificate") id . B64.decode . T.encodeUtf8

attributeFromAssertion :: T.Text -> [(T.Text,T.Text)] -> T.Text
attributeFromAssertion name = fromMaybe ($unexpectedError $ "missing field in assertion" <+> T.unpack name) . lookup name

decodeProvider :: T.Text -> EID.AuthenticationProvider
decodeProvider s = case s of
       "no_bankid"         -> EID.NetsNOBankID
       "no_bidmob"         -> EID.NetsNOBankID
       "dk_nemid_js"       -> EID.NetsDKNemID
       "dk_nemid-opensign" -> EID.NetsDKNemID
       _ -> $unexpectedError $ "provider not supported"  <+> T.unpack s

flashMessageUserHasIdentifiedWithDifferentSSN :: TemplatesMonad m => m FlashMessage
flashMessageUserHasIdentifiedWithDifferentSSN =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageUserHasIdentifiedWithDifferentSSN"

handleResolve :: Kontrakcja m => m InternalKontraResponse
handleResolve = do
  ctx <- getContext
  case (get ctxnetsconfig ctx) of
    Nothing -> do
      logAttention_ "Request to resolve nets authorization when no nets config available"
      internalError
    Just netsconf -> do
      mnt <-  getField "TARGET"
      mart <- getField "SAMLart"
      case (join $ fmap decodeNetsTarget mnt,mart) of
        (Just nt, _) | ctxDomainUrl ctx /= netsTransactionDomain nt -> do
          -- Nets can redirect us from branded domain to main domain. We need to jump back to branded domain for cookies
          link <- currentLink
          return $ internalResponse $ LinkExternal $ replace (ctxDomainUrl ctx) (netsTransactionDomain nt) link
        (Just nt, Just art) -> do
          (doc,sl) <- getDocumentAndSignatoryForEID (netsDocumentID nt) (netsSignatoryID nt)
          certErrorHandler <- mkCertErrorHandler
          debugFunction <- mkDebugFunction
          let netsAuth =  CurlAuthBasic (netsMerchantIdentifier netsconf) (netsMerchantPassword netsconf)
              transport = curlTransport SecureSSL netsAuth (T.unpack $ netsAssertionUrl netsconf) certErrorHandler debugFunction
          res <- soapCall transport "" () (GetAssertionRequest {  assertionArtifact = T.pack art }) xpGetAssertionResponse
          if ("Success" `T.isInfixOf` assertionStatusCode res)
             then do
               let provider = decodeProvider $ attributeFromAssertion "IDPROVIDER" $ assertionAttributes res
               resolve <- case provider of
                 EID.NetsNOBankID -> return handleResolveNetsNOBankID
                 EID.NetsDKNemID  -> return handleResolveNetsDKNemID
                 _ -> do
                   logAttention_ "Received invalid provider from Nets"
                   internalError
               resolvesucceeded <- resolve res doc nt sl ctx
               if resolvesucceeded
                 then do
                   logInfo_ $ "Successful assertion check with Nets. Signatory redirected back and should see view for signing"
                   return $ internalResponse $ LinkExternal $ netsReturnURL nt
                 else do
                   -- we have to switch lang here to get proper template for flash message
                   switchLang $ getLang doc
                   flashmessage <- flashMessageUserHasIdentifiedWithDifferentSSN
                   return $ internalResponseWithFlash flashmessage $ LinkExternal $ netsReturnURL nt
             else do
               logInfo "Checking assertion with Nets failed. Signatory redirected back and should see identify view." $ object [
                  "assertion_code" .= assertionStatusCode res
                 ]
               return $ internalResponse $ LinkExternal $ netsReturnURL nt
        _ -> do
          logAttention "SAML or Target missing for Nets resolve request" $ object [
              "target" .= show mnt
            , "saml_art" .= show mart
            ]
          internalError

handleResolveNetsNOBankID :: Kontrakcja m => GetAssertionResponse -> Document -> NetsTarget -> SignatoryLink -> Context -> m Bool
handleResolveNetsNOBankID res doc nt sl ctx = do
  sessionID <- get ctxsessionid <$> getContext
  let decodeInternalProvider s = case s of
         "no_bankid"         -> NetsNOBankIDStandard
         "no_bidmob"         -> NetsNOBankIDMobile
         _ -> $unexpectedError $ "internal provider not supported"  <+> T.unpack s
  let internal_provider = decodeInternalProvider $ attributeFromAssertion "IDPROVIDER" $ assertionAttributes res
  let signatoryName = attributeFromAssertion "CN" $ assertionAttributes res
  let dob = attributeFromAssertion "DOB" $ assertionAttributes res
  let dobSSN = T.pack $ take 6 $ getPersonalNumber sl
  let dobNETS = formatDOB dob
  let certificate = decodeCertificate $ attributeFromAssertion "CERTIFICATE" $ assertionAttributes res
  let mphone = lookup "NO_CEL8" $ assertionAttributes res
  let mpid = lookup "NO_BID_PID" $ assertionAttributes res

  if (dobNETS /= dobSSN)
    then do
      -- FIXME
      logAttention "Date of birth from NETS does not match date of birth from SSN. Signatory redirected back and should see identify view." $ object [
          "dob_nets" .= dobNETS
        , "dob_ssn" .= dobSSN
        ]
      return False
    else do
      -- Put NO BankID transaction in DB
      dbUpdate $ EID.MergeNetsNOBankIDAuthentication sessionID (netsSignatoryID nt) $ NetsNOBankIDAuthentication {
            netsNOBankIDInternalProvider = internal_provider
          , netsNOBankIDSignatoryName = signatoryName
          , netsNOBankIDPhoneNumber = mphone
          , netsNOBankIDDateOfBirth = dob
          , netsNOBankIDCertificate = certificate
        }

      withDocument doc $ do
        --Add evidence
        let eventFields = do
             F.value "signatory_name" signatoryName
             F.value "signatory_mobile" mphone
             F.value "signatory_dob" dob
             -- XXX signatory_pid is saved as evidence, but never used again
             F.value "signatory_pid" mpid
             F.value "signature" $ B64.encode certificate
        void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence  (eventFields) (Just sl) Nothing =<< signatoryActor ctx sl

        -- Updating phone number - mobile workflow only and only if not provided
        when (isJust mphone) $ do
          let phone = T.unpack (fromJust mphone)
          let formattedPhoneFromNets = "+47" ++ phone
          let signatoryHasFilledInPhone = getMobile sl == ""
          let formattedPhoneFromSignatory = filter (\c -> not (c `elem` (" -"::String))) $ getMobile sl
          when (not signatoryHasFilledInPhone && formattedPhoneFromSignatory /= formattedPhoneFromNets) $ do
            logAttention_ "Not matching phone for NO BankID - Nets should have blocked that"
            internalError
          when (signatoryHasFilledInPhone && Pending == documentstatus doc) $ do
            dbUpdate . UpdatePhoneAfterIdentificationToView sl phone formattedPhoneFromNets =<< signatoryActor ctx sl

      dbUpdate $ ChargeCompanyForNOBankIDAuthentication (documentid doc)
      return True


handleResolveNetsDKNemID :: Kontrakcja m => GetAssertionResponse -> Document -> NetsTarget -> SignatoryLink -> Context -> m Bool
handleResolveNetsDKNemID res doc nt sl ctx = do
  sessionID <- get ctxsessionid <$> getContext
  let decodeInternalProvider s = case s of
         "dk_nemid_js"       -> NetsDKNemIDKeyCard
         "dk_nemid-opensign" -> NetsDKNemIDKeyFile
         _ -> $unexpectedError $ "internal provider not supported"  <+> T.unpack s
      internal_provider = decodeInternalProvider $ attributeFromAssertion "IDPROVIDER" $ assertionAttributes res
      signatoryName = cnFromDN $ attributeFromAssertion "DN" $ assertionAttributes res
      ssn_sl = T.pack $ getPersonalNumber sl
      ssn_nets = attributeFromAssertion "DK_SSN" $ assertionAttributes res
      dob = dobFromDKPersonalNumber $ getPersonalNumber sl
      mpid = lookup "DK_DAN_PID" $ assertionAttributes res

  let normalizeSSN = T.filter (/= '-')
  if (normalizeSSN ssn_sl /= normalizeSSN ssn_nets)
    then do
      logAttention "SSN from NETS does not match SSN from SignatoryLink." $ object [
          "ssn_sl"   .= ssn_sl
        , "ssn_nets" .= ssn_nets
        ]
      return False
    else do
      let certificate = decodeCertificate $ attributeFromAssertion "CERTIFICATE" $ assertionAttributes res

      -- Put DK Nem ID transaction in DB
      dbUpdate $ EID.MergeNetsDKNemIDAuthentication sessionID (netsSignatoryID nt) $ NetsDKNemIDAuthentication {
            netsDKNemIDInternalProvider = internal_provider
          , netsDKNemIDSignatoryName = signatoryName
          , netsDKNemIDDateOfBirth = dob
          , netsDKNemIDCertificate = certificate
        }

      withDocument doc $ do
        --Add evidence
        let eventFields = do
             F.value "signatory_name" signatoryName
             F.value "signatory_dob" dob
             -- XXX signatory_pid is saved as evidence, but never used again
             F.value "signatory_pid" mpid
             F.value "signature" $ B64.encode certificate
        void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence  (eventFields) (Just sl) Nothing =<< signatoryActor ctx sl

      dbUpdate $ ChargeCompanyForDKNemIDAuthentication (documentid doc)
      return True

------------------------------------------

handleNetsError  :: Kontrakcja m => m Response
handleNetsError = simpleHtmlResponse =<< renderTemplate_ "netsError"

-- NETS ESigning

handleSignRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleSignRequest did slid = do
  logInfo_ "NETS SIGN start"
  conf@NetsSignConfig{..} <- do
    ctx <- getContext
    case get ctxnetssignconfig ctx of
      Nothing -> noConfigurationError "Nets ESigning"
      Just netsconf -> return netsconf
  withDocumentID did $ do
    nsoID <- newSignOrderUUID
    sess <- getCurrentSession
    tbs <- textToBeSigned =<< theDocument
    now <- currentTime
    dbQuery (GetNetsSignOrder slid) >>= \case
      Just nso | not (nsoIsCanceled nso) -> do
        logInfo "Found NetsSignOrder in progress" $ logObject_ nso
        dbUpdate . MergeNetsSignOrder $ nso { nsoIsCanceled = True }
        void $ netsCall conf (CancelOrderRequest $ nsoSignOrderID nso) xpCancelOrderResponse (show did)
      _ -> return ()
    let nso = NetsSignOrder nsoID slid (T.pack tbs) (sesID sess) (5 `minutesAfter` now) False
    host_part <- T.pack <$> getHttpsHostpart
    insOrdRs <- netsCall conf (InsertOrderRequest nso conf host_part) xpInsertOrderResponse (show did)
    getSignProcRs <- netsCall conf (GetSigningProcessesRequest nso) xpGetSigningProcessesResponse (show did)
    dbUpdate $ MergeNetsSignOrder nso
    return $ object [
        "nets_sign_url" .= gsprsSignURL getSignProcRs
      , logPair_ insOrdRs
      , logPair_ getSignProcRs
      ]

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m String
textToBeSigned doc@Document{..} = renderLocalTemplate doc "tbs" $ do
  F.value "document_title" $ documenttitle
  F.value "document_id"   $ show documentid

checkNetsSignStatus
  :: (MonadMask m, MonadBaseControl IO m, MonadIO m, DocumentMonad m, MonadLog m)
  => NetsSignConfig
  -> DocumentID
  -> SignatoryLinkID
  -> m NetsSignStatus
checkNetsSignStatus nets_conf did slid = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  if (not (isPending doc) || hasSigned (fromJust $ getSigLinkFor slid doc))
    then return NetsSignStatusAlreadySigned
    else do
      logInfo_ "Fetching signature"
      esignature <- dbQuery $ ESign.GetESignature slid
      if (isJust esignature)
        then return NetsSignStatusSuccess
        else do
          mnso <- dbQuery (GetNetsSignOrder slid)
          logInfo_ "Getting Nets sign order"
          case mnso of
            Nothing -> do
              logAttention "Document Nets signing cannot be found" $ object [
                  identifier_ slid
                ]
              return $ NetsSignStatusFailure NetsFaultExpiredTransaction
            Just nso -> do
              getOrdStRs <- netsCall nets_conf (GetOrderStatusRequest nso) xpGetOrderStatusResponse (show did)
              case gosrsOrderStatus getOrdStRs of
                CancelledByMerchant -> netsStatusFailure NetsFaultCancelledByMerchant
                Expired             -> netsStatusFailure NetsFaultExpired
                ExpiredByProxy      -> netsStatusFailure NetsFaultExpiredByProxy
                RejectedBySigner    -> netsStatusFailure NetsFaultRejectedBySigner
                Active -> do
                  logInfo "Nets Sign Order not completed yet" $ logObject_ getOrdStRs
                  return $ NetsSignStatusInProgress
                Complete -> do
                  getSdoRs <- netsCall nets_conf (GetSDORequest nso) xpGetSDOResponse (show did)
                  getSdoDetRs <- netsCall nets_conf (GetSDODetailsRequest $ gsdorsB64SDOBytes getSdoRs) xpGetSDODetailsResponse (show did)
                  logInfo "NETS Sign succeeded!" $ logObject_ getOrdStRs
                  dbUpdate $ ESign.MergeNetsNOBankIDSignature slid NetsNOBankIDSignature
                    { netsnoSignatoryName = gsdodrsSignerCN getSdoDetRs
                    , netsnoSignatoryPID = gsdodrsSignerPID getSdoDetRs
                    , netsnoSignedText = nsoTextToBeSigned nso
                    , netsnoB64SDO = gsdorsB64SDOBytes getSdoRs
                    }
                  dbUpdate $ ChargeCompanyForNOBankIDSignature (documentid doc)
                  return $ NetsSignStatusSuccess
  where
    netsStatusFailure nets_fault = do
      logInfo "Document Nets signing failed" $ object [
          identifier_ slid
        , "nets_fault" .= netsFaultText nets_fault
        ]
      return $ NetsSignStatusFailure nets_fault

handleSignError  :: Kontrakcja m => m Response
handleSignError = simpleHtmlResponse =<< renderTemplate_ "netsSignError"

handleSignExit  :: Kontrakcja m => m Response
handleSignExit = simpleHtmlResponse =<< renderTemplate_ "netsSignExit"

handleSignAbort  :: Kontrakcja m => m Response
handleSignAbort = simpleHtmlResponse =<< renderTemplate_ "netsSignAbort"
