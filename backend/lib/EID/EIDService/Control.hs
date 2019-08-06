module EID.EIDService.Control (
    eidServiceRoutes
  , startVerimiEIDServiceTransaction
  ) where

import Data.Aeson ((.=), Value, object)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import Chargeable.Model
import DB
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model.Query
import Doc.SignatoryLinkID
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import EvidenceLog.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import MinutesTime
import Routing
import Session.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils

eidServiceRoutes :: Route (Kontra Response)
eidServiceRoutes = choice [
    dir "start" $ dir "verimi"    . hPost . toK2 $ startVerimiEIDServiceTransaction
  , dir "redirect-endpoint" $ dir "verimi"    . hGet . toK2 $ redirectEndpointFromVerimiEIDServiceTransaction
  ]

startVerimiEIDServiceTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Value
startVerimiEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction start"
  (doc, _) <- getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  rd <- guardJustM $ getField "redirect"
  conf <- eidServiceConf
  tid <- createVerimiTransactionWithEIDService conf did slid rd
  turl <- startVerimiTransactionWithEIDService conf tid
  sid <- getNonTempSessionID
  now <- currentTime
  let newTransaction = EIDServiceTransaction {
      estID = tid
    , estStatus = EIDServiceTransactionStatusStarted
    , estSignatoryLinkID = slid
    , estAuthKind = mkAuthKind doc
    , estProvider = EIDServiceTransactionProviderVerimi
    , estSessionID = sid
    , estDeadline = 60 `minutesAfter` now
    }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return $ object [
      "accessUrl" .= turl
    ]


updateVerimiTransactionAfterCheck :: Kontrakcja m => SignatoryLinkID -> EIDServiceTransaction -> EIDServiceTransactionStatus -> Maybe CompleteEIDServiceTransactionData -> m EIDServiceTransactionStatus
updateVerimiTransactionAfterCheck slid est ts mctd = do
  if (estStatus est == ts)
     then return $ estStatus est
     else do
       case (ts, mctd) of
         (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let sl = fromJust $ getSigLinkFor slid doc
            if (eidtdVerifiedEmail cd == T.pack (getEmail sl))
            then do
              mergeEIDServiceTransactionWithStatus EIDServiceTransactionStatusCompleteAndSuccess
              let auth = EIDServiceVerimiAuthentication {
                     eidServiceVerimiName = eidtdName cd
                   , eidServiceVerimiVerifiedEmail = Just $ eidtdVerifiedEmail cd
                   , eidServiceVerimiVerifiedPhone = Nothing
                   }
              sessionID <- getNonTempSessionID
              dbUpdate $ MergeEIDServiceVerimiAuthentication (mkAuthKind doc) sessionID slid auth
              ctx <- getContext
              let eventFields = do
                    F.value "signatory_name" $ eidtdName cd
                    F.value "provider_verimi" True
              withDocument doc $ do
                void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence (eventFields) (Just sl) Nothing =<< signatoryActor ctx sl
                dbUpdate $ ChargeUserGroupForVerimiAuthentication (documentid doc)
              return EIDServiceTransactionStatusCompleteAndSuccess
            else do
              mergeEIDServiceTransactionWithStatus EIDServiceTransactionStatusCompleteAndFailed
              return EIDServiceTransactionStatusCompleteAndFailed
         (EIDServiceTransactionStatusCompleteAndSuccess, Nothing) -> do
           mergeEIDServiceTransactionWithStatus EIDServiceTransactionStatusCompleteAndFailed
           return EIDServiceTransactionStatusCompleteAndFailed
         _ -> do
           mergeEIDServiceTransactionWithStatus ts
           return ts
  where
    mergeEIDServiceTransactionWithStatus newstatus =  dbUpdate $ MergeEIDServiceTransaction $ est {estStatus = newstatus}


checkVerimiEIDServiceTransactionForSignatory :: Kontrakcja m => SignatoryLinkID -> m (Maybe (EIDServiceTransaction, EIDServiceTransactionStatus, Maybe CompleteEIDServiceTransactionData))
checkVerimiEIDServiceTransactionForSignatory slid = do
  conf <- eidServiceConf
  sessionID <- getNonTempSessionID
  doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
  mest <- dbQuery $ GetEIDServiceTransaction sessionID slid (mkAuthKind doc)
  case mest of
    Nothing -> return Nothing
    Just est -> do
      mtd <- checkVerimiTransactionWithEIDService conf (estID est)
      case mtd of
        (Just ts, mctd) -> return $ Just (est, ts, mctd)
        (Nothing,_) -> return Nothing

redirectEndpointFromVerimiEIDServiceTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
redirectEndpointFromVerimiEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction check"
  void $ getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad <- getAnalyticsData
  ctx <- getContext
  rd <- guardJustM $ getField "redirect"
  res <- checkVerimiEIDServiceTransactionForSignatory slid
  mts <- case res of
    Just (est, ts, mctd) -> do
      nts <- updateVerimiTransactionAfterCheck slid est ts mctd
      return $ Just nts
    _ -> return Nothing
  redirectPage <- renderTemplate "postVerimiRedirect" $ do
    F.value "redirect" rd
    F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage

eidServiceConf :: Kontrakcja m => m EIDServiceConf
eidServiceConf = do
  ctx <- getContext
  case get ctxeidserviceconf ctx of
    Nothing -> noConfigurationError "No eid service provided"
    Just conf -> return conf
