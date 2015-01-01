{-# LANGUAGE NoImplicitPrelude #-}
module EID.CGI.GRP.Control (grpRoutes) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.List
import Data.Monoid.Space
import Data.Unjson
import Happstack.Server hiding (dir)
import Happstack.StaticRouting
import Text.StringTemplates.Templates
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.StringTemplates.Fields as F

import Company.Model
import DB hiding (InternalError)
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EID.CGI.GRP.Config
import EID.CGI.GRP.Data
import EID.CGI.GRP.Transaction.Model
import EID.Signature.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import Network.SOAP.Call
import Network.SOAP.Transport.Curl
import OurPrelude
import Routing
import Templates
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Log

grpRoutes :: Route (KontraPlus Response)
grpRoutes = dir "cgi" . dir "grp" $ choice [
    dir "sign"    . hPost . toK2 $ handleSignRequest
  , dir "collect" . hGet  . toK2 $ handleCollectRequest
  ]

----------------------------------------

handleSignRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleSignRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  doc <- getDocument did slid
  mcompany_display_name <- getCompanyDisplayName doc
  tbs <- textToBeSigned doc
  pn <- getField "personal_number" `onNothing` do
    Log.mixlog_ "No personal number"
    respond404
  certErrorHandler <- mkCertErrorHandler
  let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id certErrorHandler
      req = SignRequest {
        srqPolicy = cgServiceID
      , srqDisplayName = fromMaybe cgDisplayName mcompany_display_name
      , srqPersonalNumber = T.pack pn
      , srqUserVisibleData = T.decodeUtf8 . B64.encode . BSU.fromString $ tbs
      , srqProvider = "bankid"
      }
      parser = Right <$> xpSignResponse <|> Left <$> xpGrpFault

  soapCall transport "" () req parser >>= \case
    Left fault -> return $ unjsonToJSON unjsonDef fault
    Right sr@SignResponse{..} -> do
      Log.mixlog_ $ "SOAP response:" <+> show sr
      dbUpdate $ MergeCgiGrpTransaction CgiGrpTransaction {
        cgtSignatoryLinkID = slid
      , cgtTransactionID = srsTransactionID
      , ctgTextToBeSigned = T.pack tbs
      , cgtOrderRef = srsOrderRef
      }
      return $ unjsonToJSON unjsonDef srsAutoStartToken

handleCollectRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleCollectRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  mcompany_display_name <- getDocument did slid >>= getCompanyDisplayName

  CgiGrpTransaction{..} <- dbQuery (GetCgiGrpTransaction slid) `onNothing` do
    Log.mixlog_ "No active transaction"
    respond404

  certErrorHandler <- mkCertErrorHandler
  let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id certErrorHandler
      req = CollectRequest {
        crqPolicy = cgServiceID
      , crqTransactionID = cgtTransactionID
      , crqOrderRef = cgtOrderRef
      , crqDisplayName = fromMaybe cgDisplayName mcompany_display_name
      }
      parser = Right <$> xpCollectResponse <|> Left <$> xpGrpFault

  soapCall transport "" () req parser >>= \case
    Left fault -> return $ unjsonToJSON unjsonDef fault
    Right cr@CollectResponse{..} -> do
      Log.mixlog_ $ "SOAP response:" <+> show cr
      when (crsProgressStatus == Complete) $ do
        -- all the required attributes are supposed to always
        -- be there, so bail out if this is not the case.
        dbUpdate $ MergeBankIDSignature slid BankIDSignature {
          bidsSignatoryName = just_lookup "cert.subject.cn" crsAttributes
        , bidsSignatoryPersonalNumber = just_lookup "cert.subject.serialnumber" crsAttributes
        , bidsSignedText = ctgTextToBeSigned
        , bidsSignature = mk_binary $ fromMaybe (missing "signature") crsSignature
        , bidsOcspResponse = mk_binary $ just_lookup "Validation.ocsp.response" crsAttributes
        }
      return $ unjsonToJSON unjsonDef crsProgressStatus
  where
    missing name = $unexpectedError $ "missing" <+> T.unpack name
    just_lookup name = fromMaybe (missing name) . lookup name

    invalid_b64 txt = $unexpectedError $ "invalid base64:" <+> T.unpack txt
    mk_binary txt = either (invalid_b64 txt) Binary . B64.decode . T.encodeUtf8 $ txt

----------------------------------------

-- | Fetch the document for e-signing. Checks that the document
-- is in the correct state and the signatory hasn't signed yet.
getDocument :: (MonadDB m, Log.MonadLog m, KontraMonad m, MonadThrow m,MonadBase IO m)
            => DocumentID -> SignatoryLinkID -> m Document
getDocument did slid = dbQuery (GetDocumentSessionToken slid) >>= \case
  Just mh -> do
    Log.mixlog_ "Document token found"
    doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
    when (documentstatus doc /= Pending) $ do
      Log.mixlog_ $ "Document is" <+> show (documentstatus doc) <+> ", should be" <+> show Pending
      respond404
    -- this should always succeed as we already got the document
    let slink = $fromJust $ getSigLinkFor slid doc
    when (hasSigned slink) $ do
      Log.mixlog_ "Signatory already signed the document"
      respond404
    return doc
  Nothing -> do
    Log.mixlog_ "No document token found"
    respond404

getCompanyDisplayName :: (MonadDB m, MonadThrow m) => Document -> m (Maybe T.Text)
getCompanyDisplayName doc = fmap T.pack . companycgidisplayname . companyinfo
  <$> dbQuery (GetCompanyByUserID $ $fromJust $ maybesignatory author)
  where
    author = $fromJust $ getSigLinkFor signatoryisauthor doc

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m String
textToBeSigned doc@Document{..} = renderLocalTemplate doc "tbs" $ do
  F.value "document_title" $ documenttitle
  F.value "document_id"   $ show documentid
  F.value "author_name"   $ getAuthorName doc
