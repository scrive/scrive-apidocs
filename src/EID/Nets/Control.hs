module EID.Nets.Control (netsRoutes) where


import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Text as Text
import Happstack.Server hiding (dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as Text
import qualified Text.StringTemplates.Fields as F

import AppView
import Chargeable.Model
import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model
import Doc.Tokens.Model
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
import Util.SignatoryLinkUtils

netsRoutes :: Route (Kontra Response)
netsRoutes = choice [
      dir "resolve" . hGet  . toK0 $ handleResolve
    -- Bellow are error pages - names are based on nets parameters, not what they are
    , dir "status" . hGet  . toK0 $ handleNetsError
    , dir "start"  . hGet  . toK0 $ handleNetsError
  ]

----------------------------------------

handleResolve :: Kontrakcja m => m KontraLink
handleResolve = do
  ctx <- getContext
  case (ctxnetsconfig ctx) of
    Nothing -> internalError
    Just netsconf -> do
      mnt <- join <$> (fmap decodeNetsTarget) <$> getField "TARGET"
      mart <- getField "SAMLart"
      case (mnt,mart) of
        (Just nt, Just art) -> do
          (doc,sl) <- guardDocumentAccess nt
          certErrorHandler <- mkCertErrorHandler
          debugFunction <- mkDebugFunction
          let netsAuth =  CurlAuthBasic (netsMerchantIdentifier netsconf) (netsMerchantPassword netsconf)
              transport = curlTransport SecureSSL netsAuth (Text.unpack $ netsAssertionUrl netsconf) id certErrorHandler debugFunction
          res <- soapCall transport "" () (GetAssertionRequest {  assertionArtifact = Text.pack art }) xpGetAssertionResponse
          if ("Success" `Text.isInfixOf` assertionStatusCode res)
             then do
               sessionID <- ctxsessionid <$> getContext
               let attributeFromAssestion name = fromMaybe ($unexpectedError $ "missing field in assestion" <+> Text.unpack name) . lookup name
               let decodeCertificate = either ($unexpectedError $ "invalid base64 of nets certificate") Binary . B64.decode . Text.encodeUtf8
               let decodeProvider s = case s of
                                      "no_bankid" -> NetsNOBankIDStandard
                                      "no_bidmob" -> NetsNOBankIDMobile
                                      _ -> $unexpectedError $ "provider not supported"  <+> Text.unpack s
               let provider = decodeProvider $ attributeFromAssestion "IDPROVIDER" $ assertionAttributes res
               let signatoryName = attributeFromAssestion "CN" $ assertionAttributes res
               let certificate = decodeCertificate $ attributeFromAssestion "CERTIFICATE" $ assertionAttributes res
               let mphone = lookup "NO_CEL8" $ assertionAttributes res

               -- Put NO BankID transaction in DB
               dbUpdate $ MergeNetsNOBankIDAuthentication sessionID (netsSignatoryID nt) $ NetsNOBankIDAuthentication {
                     netsNOBankIDInternalProvider = provider
                   , netsNOBankIDSignatoryName = signatoryName
                   , netsNOBankIDPhoneNumber = mphone
                   , netsNOBankIDCertificate = certificate
                 }

               withDocument doc $ do
                 --Add evidence
                 let eventFields = do
                      F.value "signatory_name" signatoryName
                      F.value "signatory_mobile" mphone
                      F.value "provider" $ ("Norwegian BankID" :: String)
                      F.value "signature" $ B64.encode . unBinary $ certificate
                 void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence  (eventFields) (Just sl) Nothing =<< signatoryActor ctx sl

                 -- Updating phone number - mobile workflow only and only if not provided
                 when (isJust mphone) $ do
                   let formattedPhone = "+47" ++ (Text.unpack ($fromJust mphone))
                   when (getMobile sl /= formattedPhone && getMobile sl /= "") $ do
                     logAttention_ "Not matching phone for NO BankID - Nets should blocked that"
                     internalError
                   dbUpdate . UpdatePhoneAfterIdentificationToView sl formattedPhone =<< signatoryActor ctx sl

               dbUpdate $ ChargeCompanyForNOBankIDAuthentication (documentid doc)

               logInfo_ $ "Successful assertion check with Nets. Signatory redirected back and should see view for signing"
               return $ LinkExternal $ netsReturnURL nt
             else do
               logInfo_ $ "Checking assertion with Nets failed. Status was " <+> Text.unpack (assertionStatusCode res) <+> ". Signatory redirected back and should see identify view."
               return $ LinkExternal $ netsReturnURL nt
        _ -> internalError

------------------------------------------

handleNetsError  :: Kontrakcja m => m Response
handleNetsError = simpleHtmlResonseClrFlash =<< renderTemplate_ "netsError"

------------------------------------------

-- | Guard that reloave action can be done from current session
guardDocumentAccess :: (MonadDB m, MonadLog m, KontraMonad m, MonadThrow m,MonadBase IO m) => NetsTarget -> m (Document,SignatoryLink)
guardDocumentAccess nt= dbQuery (GetDocumentSessionToken $ netsSignatoryID nt) >>= \case
  Just mh -> do
    logInfo_ "Document token found"
    doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash (netsDocumentID nt) (netsSignatoryID nt) mh
    when (documentstatus doc /= Pending) $ do
      logInfo_ $ "Document is" <+> show (documentstatus doc) <+> ", should be" <+> show Pending
      respond404
    -- this should always succeed as we already got the document
    let slink = $fromJust $ getSigLinkFor (netsSignatoryID nt) doc
    when (hasSigned slink) $ do
      logInfo_ "Signatory already signed the document"
      respond404
    return (doc,slink)
  Nothing -> do
    logInfo_ "No document token found"
    respond404
