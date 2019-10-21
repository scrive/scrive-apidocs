module EID.EIDService.Control (
    eidServiceRoutes
  ) where

import Data.Aeson ((.=), Value, object)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
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
import Templates (renderTextTemplate)
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils

eidServiceRoutes :: Route (Kontra Response)
eidServiceRoutes = choice [
    dir "start" $ dir "verimi" . hPost . toK2 $ startVerimiEIDServiceTransaction
  , dir "redirect-endpoint" $ dir "verimi" . hGet . toK2 $
      redirectEndpointFromVerimiEIDServiceTransaction
  , dir "start" $ dir "idin" . hPost . toK2 $ startIDINEIDServiceTransaction
  , dir "redirect-endpoint" $ dir "idin" . hGet . toK2 $
      redirectEndpointFromIDINEIDServiceTransaction
  ]

eidServiceConf :: Kontrakcja m => m EIDServiceConf
eidServiceConf = do
  ctx <- getContext
  case get ctxeidserviceconf ctx of
    Nothing   -> noConfigurationError "No eid service provided"
    Just conf -> return conf

startVerimiEIDServiceTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Value
startVerimiEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction start"
  doc  <- fst <$> getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  rd   <- guardJustM $ getField "redirect"
  conf <- eidServiceConf
  tid  <- createVerimiTransactionWithEIDService conf did slid rd
  turl <- startVerimiTransactionWithEIDService conf tid
  sid  <- getNonTempSessionID
  now  <- currentTime
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

updateVerimiTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransaction
  -> EIDServiceTransactionStatus
  -> Maybe CompleteVerimiEIDServiceTransactionData -> m EIDServiceTransactionStatus
updateVerimiTransactionAfterCheck slid est ts mctd = do
  if (estStatus est == ts)
     then return $ estStatus est
     else do
       case (ts, mctd) of
         (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let sl = fromJust $ getSigLinkFor slid doc
            if (eidvtdVerifiedEmail cd == getEmail sl)
            then do
              mergeEIDServiceTransactionWithStatus EIDServiceTransactionStatusCompleteAndSuccess
              let auth = EIDServiceVerimiAuthentication {
                     eidServiceVerimiName = eidvtdName cd
                   , eidServiceVerimiVerifiedEmail = Just $ eidvtdVerifiedEmail cd
                   , eidServiceVerimiVerifiedPhone = Nothing
                   }
              sessionID <- getNonTempSessionID
              dbUpdate $ MergeEIDServiceVerimiAuthentication (mkAuthKind doc) sessionID slid auth
              ctx <- getContext
              let eventFields = do
                    F.value "signatory_name" $ eidvtdName cd
                    F.value "provider_verimi" True
              withDocument doc $ do
                void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg
                  AuthenticatedToViewEvidence (eventFields) (Just sl) Nothing
                    =<< signatoryActor ctx sl
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
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate $ MergeEIDServiceTransaction $ est {estStatus = newstatus}

checkVerimiEIDServiceTransactionForSignatory
  :: Kontrakcja m
  => SignatoryLinkID -> m (Maybe
    ( EIDServiceTransaction
    , EIDServiceTransactionStatus
    , Maybe CompleteVerimiEIDServiceTransactionData
    ))
checkVerimiEIDServiceTransactionForSignatory slid = do
  conf      <- eidServiceConf
  sessionID <- getNonTempSessionID
  doc       <- dbQuery $ GetDocumentBySignatoryLinkID slid
  mest      <- dbQuery $ GetEIDServiceTransaction sessionID slid (mkAuthKind doc)
  case mest of
    Nothing  -> return Nothing
    Just est -> checkVerimiTransactionWithEIDService conf (estID est) >>= \case
      (Nothing, _)    -> return Nothing
      (Just ts, mctd) -> return $ Just (est, ts, mctd)

redirectEndpointFromVerimiEIDServiceTransaction
  :: Kontrakcja m
  => DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromVerimiEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction check"
  void $ getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad  <- getAnalyticsData
  ctx <- getContext
  rd  <- guardJustM $ getField "redirect"
  res <- checkVerimiEIDServiceTransactionForSignatory slid
  mts <- case res of
    Just (est, ts, mctd) -> do
      nts <- updateVerimiTransactionAfterCheck slid est ts mctd
      return $ Just nts
    _ -> return Nothing
  redirectPage <- renderTextTemplate "postVerimiRedirect" $ do
    F.value "redirect" rd
    F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage

startIDINEIDServiceTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Value
startIDINEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction start"
  doc  <- fst <$> getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  rd   <- guardJustM $ getField "redirect"
  conf <- eidServiceConf
  tid  <- createIDINTransactionWithEIDService conf did slid rd
  turl <- startIDINTransactionWithEIDService conf tid
  sid  <- getNonTempSessionID
  now  <- currentTime
  let newTransaction = EIDServiceTransaction {
      estID = tid
    , estStatus = EIDServiceTransactionStatusStarted
    , estSignatoryLinkID = slid
    , estAuthKind = mkAuthKind doc
    , estProvider = EIDServiceTransactionProviderIDIN
    , estSessionID = sid
    , estDeadline = 60 `minutesAfter` now
    }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return $ object [
      "accessUrl" .= turl
    ]

updateIDINTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransaction
  -> EIDServiceTransactionStatus
  -> Maybe CompleteIDINEIDServiceTransactionData -> m EIDServiceTransactionStatus
updateIDINTransactionAfterCheck slid est ts mctd = do
  if (estStatus est == ts)
     then return $ estStatus est
     else do
       case (ts, mctd) of
         (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let sl = fromJust $ getSigLinkFor slid doc
            mergeEIDServiceTransactionWithStatus EIDServiceTransactionStatusCompleteAndSuccess
            let auth = EIDServiceIDINAuthentication {
                    eidServiceIDINName = eiditdName cd
                  , eidServiceIDINVerifiedEmail = Just $ eiditdVerifiedEmail cd
                  , eidServiceIDINVerifiedPhone = Nothing
                  , eidServiceIDINBirthDate = Just $ eiditdBirthDate cd
                  , eidServiceIDINCustomerID = Just $ eiditCumstomerID cd
                  }
            sessionID <- getNonTempSessionID
            dbUpdate $ MergeEIDServiceIDINAuthentication (mkAuthKind doc) sessionID slid auth
            ctx <- getContext
            let eventFields = do
                  F.value "signatory_name" $ eiditdName cd
                  F.value "provider_idin" True
            withDocument doc $ do
              void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg
                AuthenticatedToViewEvidence (eventFields) (Just sl) Nothing
                  =<< signatoryActor ctx sl
              dbUpdate $ ChargeUserGroupForIDINAuthentication (documentid doc)
            return EIDServiceTransactionStatusCompleteAndSuccess
         (EIDServiceTransactionStatusCompleteAndSuccess, Nothing) -> do
           mergeEIDServiceTransactionWithStatus EIDServiceTransactionStatusCompleteAndFailed
           return EIDServiceTransactionStatusCompleteAndFailed
         _ -> do
           mergeEIDServiceTransactionWithStatus ts
           return ts
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate $ MergeEIDServiceTransaction $ est {estStatus = newstatus}

checkIDINEIDServiceTransactionForSignatory
  :: Kontrakcja m
  => SignatoryLinkID -> m (Maybe
    ( EIDServiceTransaction
    , EIDServiceTransactionStatus
    , Maybe CompleteIDINEIDServiceTransactionData
    ))
checkIDINEIDServiceTransactionForSignatory slid = do
  conf      <- eidServiceConf
  sessionID <- getNonTempSessionID
  doc       <- dbQuery $ GetDocumentBySignatoryLinkID slid
  mest      <- dbQuery $ GetEIDServiceTransaction sessionID slid (mkAuthKind doc)
  case mest of
    Nothing  -> return Nothing
    Just est -> checkIDINTransactionWithEIDService conf (estID est) >>= \case
      (Nothing, _)    -> return Nothing
      (Just ts, mctd) -> return $ Just (est, ts, mctd)

redirectEndpointFromIDINEIDServiceTransaction
  :: Kontrakcja m
  => DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromIDINEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction check"
  void $ getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad  <- getAnalyticsData
  ctx <- getContext
  rd  <- guardJustM $ getField "redirect"
  res <- checkIDINEIDServiceTransactionForSignatory slid
  mts <- case res of
    Just (est, ts, mctd) -> do
      nts <- updateIDINTransactionAfterCheck slid est ts mctd
      return $ Just nts
    _ -> return Nothing
  redirectPage <- renderTextTemplate "postIDINRedirect" $ do
    F.value "redirect" rd
    F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage
