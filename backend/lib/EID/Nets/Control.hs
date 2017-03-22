module EID.Nets.Control (netsRoutes) where

import Control.Monad
import Data.String.Utils (replace)
import Happstack.Server hiding (dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.StringTemplates.Fields as F

import AppView
import Chargeable.Model
import DB
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentMonad
import Doc.Model
import EID.Authentication.Model
import EID.Nets.Config
import EID.Nets.Data
import EvidenceLog.Model
import FlashMessage
import Happstack.Fields
import InternalResponse
import Kontra hiding (InternalError)
import KontraLink
import KontraPrelude
import Network.SOAP.Call
import Network.SOAP.Transport.Curl
import Routing
import Util.Actor
import Util.HasSomeUserInfo
import Utils.HTTP

netsRoutes :: Route (Kontra Response)
netsRoutes = choice [
      dir "resolve" . hGet  . toK0 $ handleResolve
    -- Bellow are error pages - names are based on nets parameters, not what they are
    , dir "status" . hGet  . toK0 $ handleNetsError
    , dir "start"  . hGet  . toK0 $ handleNetsError
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
  extractCN $ lookup "CN" $ fmap parsePair $ T.splitOn ", " $ dn
    where
      parsePair s = case T.splitOn "=" s of
        (name:values) -> (name, T.intercalate "=" values)
        _ -> $unexpectedError $ "Cannot parse DN value: " <> show dn
      extractCN (Just s) = case T.splitOn " + " s of
        (cn:_) -> cn
        _      -> s
      extractCN Nothing = $unexpectedError $ "Cannot parse DN value: " <> show dn

decodeCertificate :: T.Text -> B.ByteString
decodeCertificate = either ($unexpectedError $ "invalid base64 of nets certificate") id . B64.decode . T.encodeUtf8

attributeFromAssertion :: T.Text -> [(T.Text,T.Text)] -> T.Text
attributeFromAssertion name = fromMaybe ($unexpectedError $ "missing field in assertion" <+> T.unpack name) . lookup name

decodeProvider :: T.Text -> AuthenticationProvider
decodeProvider s = case s of
       "no_bankid"         -> NetsNOBankID
       "no_bidmob"         -> NetsNOBankID
       "dk_nemid_js"       -> NetsDKNemID
       "dk_nemid-opensign" -> NetsDKNemID
       _ -> $unexpectedError $ "provider not supported"  <+> T.unpack s

flashMessageUserHasIdentifiedWithDifferentSSN :: TemplatesMonad m => m FlashMessage
flashMessageUserHasIdentifiedWithDifferentSSN =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageUserHasIdentifiedWithDifferentSSN"

handleResolve :: Kontrakcja m => m InternalKontraResponse
handleResolve = do
  ctx <- getContext
  case (ctxnetsconfig ctx) of
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
              transport = curlTransport SecureSSL netsAuth (T.unpack $ netsAssertionUrl netsconf) return certErrorHandler debugFunction
          res <- soapCall transport "" () (GetAssertionRequest {  assertionArtifact = T.pack art }) xpGetAssertionResponse
          if ("Success" `T.isInfixOf` assertionStatusCode res)
             then do
               let provider = decodeProvider $ attributeFromAssertion "IDPROVIDER" $ assertionAttributes res
               resolve <- case provider of
                 NetsNOBankID -> return handleResolveNetsNOBankID
                 NetsDKNemID  -> return handleResolveNetsDKNemID
                 _ -> do
                   logAttention_ "Received invalid provider from Nets"
                   internalError
               identifyssnerror <- resolve res doc nt sl ctx
               if identifyssnerror
                 then do
                   flashmessage <- flashMessageUserHasIdentifiedWithDifferentSSN
                   return $ internalResponseWithFlash flashmessage $ LinkExternal $ netsReturnURL nt
                 else do
                   logInfo_ $ "Successful assertion check with Nets. Signatory redirected back and should see view for signing"
                   return $ internalResponse $ LinkExternal $ netsReturnURL nt
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
  sessionID <- ctxsessionid <$> getContext
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
      return True
    else do
      -- Put NO BankID transaction in DB
      dbUpdate $ MergeNetsNOBankIDAuthentication sessionID (netsSignatoryID nt) $ NetsNOBankIDAuthentication {
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
      return False


handleResolveNetsDKNemID :: Kontrakcja m => GetAssertionResponse -> Document -> NetsTarget -> SignatoryLink -> Context -> m Bool
handleResolveNetsDKNemID res doc nt sl ctx = do
  sessionID <- ctxsessionid <$> getContext
  let decodeInternalProvider s = case s of
         "dk_nemid_js"       -> NetsDKNemIDKeyCard
         "dk_nemid-opensign" -> NetsDKNemIDKeyFile
         _ -> $unexpectedError $ "internal provider not supported"  <+> T.unpack s
      internal_provider = decodeInternalProvider $ attributeFromAssertion "IDPROVIDER" $ assertionAttributes res
      signatoryName = cnFromDN $ attributeFromAssertion "DN" $ assertionAttributes res
      ssn_sl = T.pack $ getPersonalNumber sl
      ssn_nets = attributeFromAssertion "DK_SSN" $ assertionAttributes res
      dob = dobFromDKPersonalNumber $ getPersonalNumber sl
      mpid = lookup "DN_DAN_PID" $ assertionAttributes res

  let normalizeSSN = T.filter (/= '-')
  if (normalizeSSN ssn_sl /= normalizeSSN ssn_nets)
    then do
      logAttention "SSN from NETS does not match SSN from SignatoryLink." $ object [
          "ssn_sl"   .= ssn_sl
        , "ssn_nets" .= ssn_nets
        ]
      return True
    else do
      let certificate = decodeCertificate $ attributeFromAssertion "CERTIFICATE" $ assertionAttributes res

      -- Put DK Nem ID transaction in DB
      dbUpdate $ MergeNetsDKNemIDAuthentication sessionID (netsSignatoryID nt) $ NetsDKNemIDAuthentication {
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
      return False

------------------------------------------

handleNetsError  :: Kontrakcja m => m Response
handleNetsError = simpleHtmlResponse =<< renderTemplate_ "netsError"
