module EID.Nets.Control (netsRoutes) where

import Control.Monad
import Data.String.Utils (replace)
import Happstack.Server hiding (dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
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
import Happstack.Fields
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

handleResolve :: Kontrakcja m => m KontraLink
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
          return $ LinkExternal $ replace (ctxDomainUrl ctx) (netsTransactionDomain nt) link
        (Just nt, Just art) -> do
          (doc,sl) <- getDocumentAndSignatoryForEID (netsDocumentID nt) (netsSignatoryID nt)
          certErrorHandler <- mkCertErrorHandler
          debugFunction <- mkDebugFunction
          let netsAuth =  CurlAuthBasic (netsMerchantIdentifier netsconf) (netsMerchantPassword netsconf)
              transport = curlTransport SecureSSL netsAuth (T.unpack $ netsAssertionUrl netsconf) return certErrorHandler debugFunction
          res <- soapCall transport "" () (GetAssertionRequest {  assertionArtifact = T.pack art }) xpGetAssertionResponse
          if ("Success" `T.isInfixOf` assertionStatusCode res)
             then do
               sessionID <- ctxsessionid <$> getContext
               let attributeFromAssertion name = fromMaybe ($unexpectedError $ "missing field in assertion" <+> T.unpack name) . lookup name
               let decodeCertificate = either ($unexpectedError $ "invalid base64 of nets certificate") id . B64.decode . T.encodeUtf8
               let decodeProvider s = case s of
                                      "no_bankid" -> NetsNOBankIDStandard
                                      "no_bidmob" -> NetsNOBankIDMobile
                                      _ -> $unexpectedError $ "provider not supported"  <+> T.unpack s
               let provider = decodeProvider $ attributeFromAssertion "IDPROVIDER" $ assertionAttributes res
               let signatoryName = attributeFromAssertion "CN" $ assertionAttributes res
               let dob = attributeFromAssertion "DOB" $ assertionAttributes res
               let dobSSN = T.pack $ take 6 $ getPersonalNumber sl
               let dobNETS = formatDOB dob
               let certificate = decodeCertificate $ attributeFromAssertion "CERTIFICATE" $ assertionAttributes res
               let mphone = lookup "NO_CEL8" $ assertionAttributes res
               let mpid = lookup "NO_BID_PID" $ assertionAttributes res

               when (dobNETS /= dobSSN) $ do
                  -- FIXME
                  logAttention "Date of birth from NETS does not match date of birth from SSN." $ object [
                      "DOB" .= dobNETS
                    , "SSN" .= dobSSN
                    ]
                  internalError

               -- Put NO BankID transaction in DB
               dbUpdate $ MergeNetsNOBankIDAuthentication sessionID (netsSignatoryID nt) $ NetsNOBankIDAuthentication {
                     netsNOBankIDInternalProvider = provider
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
                      F.value "signatory_pid" mpid
                      F.value "provider_nobankid" True
                      F.value "signature" $ B64.encode certificate
                 void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence  (eventFields) (Just sl) Nothing =<< signatoryActor ctx sl


                 -- Updating phone number - mobile workflow only and only if not provided
                 when (isJust mphone) $ do
                   let phone = T.unpack ($fromJust mphone)
                   let formattedPhoneFromNets = "+47" ++ phone
                   let signatoryHasFilledInPhone = getMobile sl == ""
                   let formattedPhoneFromSignatory = filter (\c -> not (c `elem` (" -"::String))) $ getMobile sl
                   when (not signatoryHasFilledInPhone && formattedPhoneFromSignatory /= formattedPhoneFromNets) $ do
                     logAttention_ "Not matching phone for NO BankID - Nets should have blocked that"
                     internalError
                   when (signatoryHasFilledInPhone && Pending == documentstatus doc) $ do
                     dbUpdate . UpdatePhoneAfterIdentificationToView sl phone formattedPhoneFromNets =<< signatoryActor ctx sl

               dbUpdate $ ChargeCompanyForNOBankIDAuthentication (documentid doc)

               logInfo_ $ "Successful assertion check with Nets. Signatory redirected back and should see view for signing"
               return $ LinkExternal $ netsReturnURL nt
             else do
               logInfo "Checking assertion with Nets failed. Signatory redirected back and should see identify view." $ object [
                  "assertion_code" .= assertionStatusCode res
                 ]
               return $ LinkExternal $ netsReturnURL nt
        _ -> do
          logAttention "SAML or Target missing for Nets resolve request" $ object [
              "TARGET" .= show mnt
            , "SAMLart" .= show mart
            ]
          internalError
------------------------------------------

handleNetsError  :: Kontrakcja m => m Response
handleNetsError = simpleHtmlResonseClrFlash =<< renderTemplate_ "netsError"
