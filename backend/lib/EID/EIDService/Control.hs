module EID.EIDService.Control (
    eidServiceRoutes
  ) where

import Data.Aeson (Value)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import Network.HTTP.Base (urlEncode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import DB
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocUtils
import Doc.SignatoryLinkID
import EID.EIDService.Model
import EID.EIDService.Provider
import EID.EIDService.Types
import Happstack.Fields
import Kontra hiding (InternalError)
import MinutesTime
import Routing
import Session.Model
import Templates (renderTextTemplate)
import Util.MonadUtils

eidServiceRoutes :: Route (Kontra Response)
eidServiceRoutes = choice
  -- start endpoints
  [ dir "start" . hPost . toK4 $ startEIDServiceTransaction
  -- redirect endpoints
  , dir "redirect-endpoint" . hGet . toK4 $ redirectEndpointFromEIDServiceTransaction
  ]

-- Start endpoints for auth and sign

startEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> EIDServiceEndpointType
  -> DocumentID
  -> SignatoryLinkID
  -> m Value
startEIDServiceTransaction provider epType did slid = do
  let providerName = toEIDServiceProviderName provider
  logInfo_ $ "EID Service transaction start - for " <> providerName
  (doc, sl) <- getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  let authKind = case epType of
        EIDServiceAuthEndpoint -> EIDServiceAuthToView $ mkAuthKind doc
        EIDServiceSignEndpoint -> EIDServiceAuthToSign
  conf               <- eidServiceConf doc
  (tid, val, status) <- beginEIDServiceTransaction conf provider authKind doc sl
  sid                <- getNonTempSessionID
  now                <- currentTime
  let newTransaction = EIDServiceTransactionFromDB
        { estID              = tid
        , estStatus          = status
        , estSignatoryLinkID = signatorylinkid sl
        , estAuthKind        = authKind
        , estProvider        = provider
        , estSessionID       = sid
        , estDeadline        = 60 `minutesAfter` now
        }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return val

-- Redirect endpoints

redirectEndpointFromEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> EIDServiceEndpointType
  -> DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromEIDServiceTransaction provider authKind did slid = case authKind of
  EIDServiceAuthEndpoint ->
    redirectEndpointFromEIDServiceAuthTransaction provider did slid
  EIDServiceSignEndpoint ->
    redirectEndpointFromEIDServiceSignTransaction provider did slid

-- Auth redirect endpoints

redirectEndpointFromEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromEIDServiceAuthTransaction provider did slid = do
  logInfo_ "EID Service transaction check"
  (doc, sl)    <- getDocumentAndSignatoryForEIDAuth did slid -- access guard
  conf         <- eidServiceConf doc
  ad           <- getAnalyticsData
  ctx          <- getContext
  rd           <- guardJustM $ getField "redirect"
  mts          <- completeEIDServiceAuthTransaction conf provider doc sl
  redirectPage <- renderTextTemplate "postEIDAuthRedirect" $ do
    F.value "redirect" rd
    F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage

-- Sign redirect endpoints

redirectEndpointFromEIDServiceSignTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromEIDServiceSignTransaction provider did slid = do
  logInfo_ "EID Service signing transaction check"
  (doc, sl) <- getDocumentAndSignatoryForEIDAuth did slid -- access guard
  conf      <- eidServiceConf doc
  ad        <- getAnalyticsData
  ctx       <- getContext
  isCorrect <- completeEIDServiceSignTransaction conf provider sl
  let redirectUrl = "/s/" <> show did <> "/" <> show slid
  redirectPage <- renderTextTemplate "postEIDAuthAtSignRedirect" $ do
    F.value "redirect" . B64.encode . BSC8.pack . urlEncode $ redirectUrl
    F.value "incorrect_data" $ not isCorrect
    F.value "document_id" $ show did
    F.value "signatory_link_id" $ show slid
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage
