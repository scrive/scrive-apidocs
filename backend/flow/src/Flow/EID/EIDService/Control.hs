module Flow.EID.EIDService.Control (
    eidServiceRoutes
  ) where

import Data.Aeson (Value)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import DB
import Doc.DocStateData
import Doc.DocStateQuery
import EID.EIDService.Model (eidServiceConf)
import EID.EIDService.Types hiding (EIDServiceTransactionFromDB(..))
import Flow.Aggregator
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Provider
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Model
import Flow.Model.Types
import Flow.Names
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
  [ dir "start" . hPost . toK3 $ startEIDServiceTransaction
  -- redirect endpoints
  , dir "redirect-endpoint" . hGet . toK3 $ redirectEndpointFromEIDServiceTransaction
  ]

-- newtype UserName here so that we don't taint flow code with Happstack
newtype LocalUserName = LocalUserName UserName

instance FromReqURI LocalUserName where
  fromReqURI = Just . LocalUserName . unsafeName . T.pack

startEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> InstanceId
  -> LocalUserName
  -> m Value
startEIDServiceTransaction provider instanceId (LocalUserName userName) = do
  let providerName = toEIDServiceProviderName provider
  logInfo_ $ "EID Service transaction start - for " <> providerName
  (sl , did)   <- getAnyDocumentWithSl instanceId userName
  (doc, _  )   <- getDocumentAndSignatoryForEIDAuth did (signatorylinkid sl) -- also access guard
  fullInstance <- fromJust <$> selectFullInstance instanceId
  let authKind = if isComplete $ instanceToAggregator fullInstance
        then AuthenticationToView
        else AuthenticationToViewArchived
  conf               <- eidServiceConf doc
  (tid, val, status) <- beginEIDServiceTransaction conf
                                                   provider
                                                   authKind
                                                   instanceId
                                                   userName
  sid <- getNonTempSessionID
  now <- currentTime
  let newTransaction = EIDServiceTransactionFromDB
        { estID         = tid
        , estStatus     = status
        , estInstanceId = instanceId
        , estUserName   = userName
        , estAuthKind   = EIDServiceAuthToView authKind
        , estProvider   = provider
        , estSessionID  = sid
        , estDeadline   = 60 `minutesAfter` now
        }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return val

redirectEndpointFromEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> InstanceId
  -> LocalUserName
  -> m Response
redirectEndpointFromEIDServiceTransaction provider instanceId (LocalUserName userName) =
  do
    logInfo_ "EID Service transaction check"
    -- TODO nicer logging
    logInfo_ $ "InstanceId: " <> showt instanceId
    logInfo_ $ "User name: " <> showt userName
    (sl , did)   <- getAnyDocumentWithSl instanceId userName
    (doc, _  )   <- getDocumentAndSignatoryForEIDAuth did (signatorylinkid sl) -- also access guard
    conf         <- eidServiceConf doc
    ad           <- getAnalyticsData
    ctx          <- getContext
    rd           <- guardJustM $ getField "redirect"
    mts          <- completeEIDServiceAuthTransaction conf provider instanceId userName
    redirectPage <- renderTextTemplate "postEIDAuthRedirect" $ do
      F.value "redirect" rd
      F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
      standardPageFields ctx Nothing ad
    simpleHtmlResponse redirectPage
