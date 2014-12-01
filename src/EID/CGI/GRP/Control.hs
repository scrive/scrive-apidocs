{-# LANGUAGE NoImplicitPrelude #-}
module EID.CGI.GRP.Control (grpRoutes) where

import Control.Applicative
import Control.Monad
import Data.List
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

import DB hiding (InternalError)
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EID.CGI.GRP.Config
import EID.CGI.GRP.Data
import EID.CGI.GRP.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import Network.SOAP.Call
import Network.SOAP.Transport.Curl
import OurPrelude
import Routing
import Templates
import Util.HasSomeUserInfo
import qualified Log

grpRoutes :: Route (KontraPlus Response)
grpRoutes = dir "cgi" . dir "grp" $ choice [
    dir "sign"    . hPost . toK2 $ handleSignRequest
  , dir "collect" . hGet . toK2 $ handleCollectRequest
  ]

----------------------------------------

handleSignRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleSignRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  doc <- getDocument did slid
  tbs <- textToBeSigned doc
  pn <- getField "personal_number" `onNothing` do
    Log.mixlog_ "No personal number"
    respond404

  let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id
      req = SignRequest {
        srqPolicy = cgServiceID
      , srqDisplayName = cgDisplayName
      , srqPersonalNumber = T.pack pn
      , srqUserVisibleData = T.decodeUtf8 . B64.encode . BSU.fromString $ tbs
      , srqProvider = "bankid"
      }
      parser = Right <$> xpSignResponse <|> Left <$> xpGrpFault

  soapCall transport "" () req parser >>= \case
    Left fault -> return $ unjsonToJSON unjsonDef fault
    Right SignResponse{..} -> do
      dbUpdate $ MergeCgiGrpTransaction CgiGrpTransaction {
        cgtSignatoryLinkID = slid
      , cgtTransactionID = srsTransactionID
      , cgtOrderRef = srsOrderRef
      }
      return $ unjsonToJSON unjsonDef srsAutoStartToken

handleCollectRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleCollectRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  void $ getDocument did slid
  CgiGrpTransaction{..} <- dbQuery (GetCgiGrpTransaction slid)
    `onNothing` (Log.mixlog_ "No active transaction" >> respond404)

  let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id
      req = CollectRequest {
        crqPolicy = cgServiceID
      , crqTransactionID = cgtTransactionID
      , crqOrderRef = cgtOrderRef
      , crqDisplayName = cgDisplayName
      }
      parser = Right <$> xpCollectResponse <|> Left <$> xpGrpFault

  soapCall transport "" () req parser >>= \case
    Left fault -> return $ unjsonToJSON unjsonDef fault
    Right cr@CollectResponse{..} -> do
      Log.mixlog_ $ "RESPONSE: " ++ show cr
      return $ unjsonToJSON unjsonDef crsProgressStatus

----------------------------------------

-- | Fetch the document if it can be accessed by the signatory, i.e. if
-- there exists appropriate document token or the author is logged in.
getDocument :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Document
getDocument did slid = dbQuery (GetDocumentSessionToken slid) >>= \case
  Just mh -> do
    Log.mixlog_ "TOKEN FOUND"
    dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh)
  Nothing -> do
    Log.mixlog_ "TOKEN NOT FOUND"
    getDocByDocIDForAuthor did

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m String
textToBeSigned doc@Document{..} = renderLocalTemplate doc "tbs" $ do
  F.value "documentname"   $ documenttitle
  F.value "documentnumber" $ show documentid
  F.value "tbssigentries"  $ intercalate "\n" $ map getSigEntry signatories
  where
    signatories = filter signatoryispartner documentsignatorylinks
    getSigEntry signatory = unwords [
        getFirstName signatory
      , getLastName signatory ++ ","
      , getPersonalNumber signatory
      ]

-- | Temporary solution for the sillyness of guardJustM.
onNothing :: Monad m => m (Maybe a) -> m a -> m a
onNothing action handleNothing = maybe handleNothing return =<< action
