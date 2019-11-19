module EID.EIDService.Control (
    eidServiceRoutes
  ) where

import Data.Aeson ((.=), Value, object)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import Network.HTTP.Base (urlEncode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
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
import Session.SessionID
import Templates (renderTextTemplate)
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils

eidServiceRoutes :: Route (Kontra Response)
eidServiceRoutes = choice
  [ dir "start" . dir "verimi" . hPost . toK2 $ startVerimiEIDServiceTransaction
  , dir "redirect-endpoint"
  . dir "verimi"
  . hGet
  . toK2
  $ redirectEndpointFromVerimiEIDServiceTransaction
  , dir "start" . dir "idin-view" . hPost . toK2 $ startIDINViewEIDServiceTransaction
  , dir "redirect-endpoint"
  . dir "idin-view"
  . hGet
  . toK2
  $ redirectEndpointFromIDINViewEIDServiceTransaction
  , dir "start" . dir "idin-sign" . hPost . toK2 $ startIDINSignEIDServiceTransaction
  , dir "redirect-endpoint"
  . dir "idin-sign"
  . hGet
  . toK2
  $ redirectEndpointFromIDINSignEIDServiceTransaction
  ]

eidServiceConf :: Kontrakcja m => Document -> m EIDServiceConf
eidServiceConf doc = do
  ctx <- getContext
  case ctx ^. #eidServiceConf of
    Nothing    -> noConfigurationError "No eid service provided"
    Just conf0 -> do
      let err =
            unexpectedError $ "Impossible happened - no author for document: " <> showt
              (documentid doc)
      authorid <- maybe err return $ maybesignatory =<< getAuthorSigLink doc
      ugwp     <- dbQuery . UserGroupGetWithParentsByUserID $ authorid
      return $ case ugwpSettings ugwp ^. #eidServiceToken of
        Nothing    -> conf0
        Just token -> set #eidServiceToken token conf0

startVerimiEIDServiceTransaction
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Value
startVerimiEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction start"
  doc  <- fst <$> getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  rd   <- guardJustM $ getField "redirect"
  conf <- eidServiceConf doc

  tid  <- createVerimiTransactionWithEIDService conf did slid rd
  turl <- startVerimiTransactionWithEIDService conf tid
  sid  <- getNonTempSessionID
  now  <- currentTime
  let newTransaction = EIDServiceTransaction
        { estID              = tid
        , estStatus          = EIDServiceTransactionStatusStarted
        , estSignatoryLinkID = slid
        , estAuthKind        = EIDServiceAuthToView $ mkAuthKind doc
        , estProvider        = EIDServiceTransactionProviderVerimi
        , estSessionID       = sid
        , estDeadline        = 60 `minutesAfter` now
        }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return $ object ["accessUrl" .= turl]

updateVerimiTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransaction
  -> EIDServiceTransactionStatus
  -> Maybe CompleteVerimiEIDServiceTransactionData
  -> m EIDServiceTransactionStatus
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
              mergeEIDServiceTransactionWithStatus
                EIDServiceTransactionStatusCompleteAndSuccess
              let auth = EIDServiceVerimiAuthentication
                    { eidServiceVerimiName          = eidvtdName cd
                    , eidServiceVerimiVerifiedEmail = Just $ eidvtdVerifiedEmail cd
                    , eidServiceVerimiVerifiedPhone = Nothing
                    }
              sessionID <- getNonTempSessionID
              dbUpdate $ MergeEIDServiceVerimiAuthentication (mkAuthKind doc)
                                                             sessionID
                                                             slid
                                                             auth
              ctx <- getContext
              let eventFields = do
                    F.value "signatory_name" $ eidvtdName cd
                    F.value "provider_verimi" True
              withDocument doc $ do
                void
                  $   dbUpdate
                  .   InsertEvidenceEventWithAffectedSignatoryAndMsg
                        AuthenticatedToViewEvidence
                        (eventFields)
                        (Just sl)
                        Nothing
                  =<< signatoryActor ctx sl
                dbUpdate $ ChargeUserGroupForVerimiAuthentication (documentid doc)
              return EIDServiceTransactionStatusCompleteAndSuccess
            else do
              mergeEIDServiceTransactionWithStatus
                EIDServiceTransactionStatusCompleteAndFailed
              return EIDServiceTransactionStatusCompleteAndFailed
        (EIDServiceTransactionStatusCompleteAndSuccess, Nothing) -> do
          mergeEIDServiceTransactionWithStatus
            EIDServiceTransactionStatusCompleteAndFailed
          return EIDServiceTransactionStatusCompleteAndFailed
        _ -> do
          mergeEIDServiceTransactionWithStatus ts
          return ts
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate $ MergeEIDServiceTransaction $ est { estStatus = newstatus }

checkVerimiEIDServiceTransactionForSignatory
  :: Kontrakcja m
  => SignatoryLinkID
  -> m
       ( Maybe
           ( EIDServiceTransaction
           , EIDServiceTransactionStatus
           , Maybe CompleteVerimiEIDServiceTransactionData
           )
       )
checkVerimiEIDServiceTransactionForSignatory slid = do
  sessionID <- getNonTempSessionID
  doc       <- dbQuery $ GetDocumentBySignatoryLinkID slid
  conf      <- eidServiceConf doc
  mest      <- dbQuery $ GetEIDServiceTransactionGuardSessionID
    sessionID
    slid
    (EIDServiceAuthToView $ mkAuthKind doc)
  case mest of
    Nothing  -> return Nothing
    Just est -> checkVerimiTransactionWithEIDService conf (estID est) >>= \case
      (Nothing, _   ) -> return Nothing
      (Just ts, mctd) -> return $ Just (est, ts, mctd)

redirectEndpointFromVerimiEIDServiceTransaction
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
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

startIDINViewEIDServiceTransaction
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Value
startIDINViewEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction start - for view"
  doc  <- fst <$> getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  conf <- eidServiceConf doc
  rd   <- guardJustM $ getField "redirect"
  ctx  <- getContext
  let redirectUrl =
        (ctx ^. #brandedDomain % #url)
          <> "/eid-service/redirect-endpoint/idin-view/"
          <> showt did
          <> "/"
          <> showt slid
          <> "?redirect="
          <> rd
  startIDINEIDServiceTransaction conf
                                 (EIDServiceAuthToView $ mkAuthKind doc)
                                 slid
                                 redirectUrl

startIDINSignEIDServiceTransaction
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Value
startIDINSignEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction start - for sign"
  doc  <- fst <$> getDocumentAndSignatoryForEIDAuth did slid -- access guard
  conf <- eidServiceConf doc
  ctx  <- getContext
  let redirectUrl =
        (ctx ^. #brandedDomain % #url)
          <> "/eid-service/redirect-endpoint/idin-sign/"
          <> showt did
          <> "/"
          <> showt slid
  startIDINEIDServiceTransaction conf EIDServiceAuthToSign slid redirectUrl

startIDINEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> SignatoryLinkID
  -> Text
  -> m Value
startIDINEIDServiceTransaction conf eidserviceAuthKind slid redirectUrl = do
  tid  <- createIDINTransactionWithEIDService conf redirectUrl
  turl <- startIDINTransactionWithEIDService conf tid
  sid  <- getNonTempSessionID
  now  <- currentTime
  let newTransaction = EIDServiceTransaction
        { estID              = tid
        , estStatus          = EIDServiceTransactionStatusStarted
        , estSignatoryLinkID = slid
        , estAuthKind        = eidserviceAuthKind
        , estProvider        = EIDServiceTransactionProviderIDIN
        , estSessionID       = sid
        , estDeadline        = 60 `minutesAfter` now
        }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return $ object ["accessUrl" .= turl]

updateIDINTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransaction
  -> EIDServiceTransactionStatus
  -> Maybe CompleteIDINEIDServiceTransactionData
  -> m EIDServiceTransactionStatus
updateIDINTransactionAfterCheck slid est ts mctd = do
  if (estStatus est == ts)
    then return $ estStatus est
    else do
      case (ts, mctd) of
        (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> do
          doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
          let sl = fromJust $ getSigLinkFor slid doc
          mergeEIDServiceTransactionWithStatus
            EIDServiceTransactionStatusCompleteAndSuccess
          let auth = EIDServiceIDINAuthentication
                { eidServiceIDINName          = eiditdName cd
                , eidServiceIDINVerifiedEmail = Just $ eiditdVerifiedEmail cd
                , eidServiceIDINVerifiedPhone = Nothing
                , eidServiceIDINBirthDate     = Just $ eiditdBirthDate cd
                , eidServiceIDINCustomerID    = Just $ eiditdCustomerID cd
                }
          sessionID <- getNonTempSessionID
          dbUpdate
            $ MergeEIDServiceIDINAuthentication (mkAuthKind doc) sessionID slid auth
          ctx <- getContext
          let eventFields = do
                F.value "signatory_name" $ eiditdName cd
                F.value "provider_idin" True
          withDocument doc $ do
            void
              $   dbUpdate
              .   InsertEvidenceEventWithAffectedSignatoryAndMsg
                    AuthenticatedToViewEvidence
                    (eventFields)
                    (Just sl)
                    Nothing
              =<< signatoryActor ctx sl
            dbUpdate $ ChargeUserGroupForIDINAuthentication (documentid doc)
          return EIDServiceTransactionStatusCompleteAndSuccess
        (EIDServiceTransactionStatusCompleteAndSuccess, Nothing) -> do
          mergeEIDServiceTransactionWithStatus
            EIDServiceTransactionStatusCompleteAndFailed
          return EIDServiceTransactionStatusCompleteAndFailed
        _ -> do
          mergeEIDServiceTransactionWithStatus ts
          return ts
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate $ MergeEIDServiceTransaction $ est { estStatus = newstatus }

checkIDINEIDServiceTransactionForSignatory
  :: Kontrakcja m
  => Document
  -> SignatoryLinkID
  -> EIDServiceAuthenticationKind
  -> m
       ( Maybe
           ( EIDServiceTransaction
           , EIDServiceTransactionStatus
           , Maybe CompleteIDINEIDServiceTransactionData
           )
       )
checkIDINEIDServiceTransactionForSignatory doc slid eidserviceAuthKind = do
  conf      <- eidServiceConf doc
  sessionID <- getNonTempSessionID
  checkIDINEIDServiceTransactionForSignatoryWithConf conf
                                                     slid
                                                     sessionID
                                                     eidserviceAuthKind

checkIDINEIDServiceTransactionForSignatoryWithConf
  :: Kontrakcja m
  => EIDServiceConf
  -> SignatoryLinkID
  -> SessionID
  -> EIDServiceAuthenticationKind
  -> m
       ( Maybe
           ( EIDServiceTransaction
           , EIDServiceTransactionStatus
           , Maybe CompleteIDINEIDServiceTransactionData
           )
       )
checkIDINEIDServiceTransactionForSignatoryWithConf conf slid sessionID eidAuthKind = do
  mest <- dbQuery $ GetEIDServiceTransactionGuardSessionID sessionID slid eidAuthKind
  case mest of
    Nothing  -> return Nothing
    Just est -> checkIDINTransactionWithEIDService conf (estID est) >>= \case
      (Nothing, _   ) -> return Nothing
      (Just ts, mctd) -> return $ Just (est, ts, mctd)

redirectEndpointFromIDINViewEIDServiceTransaction
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
redirectEndpointFromIDINViewEIDServiceTransaction did slid = do
  logInfo_ "EID Service transaction check"
  void $ getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad  <- getAnalyticsData
  ctx <- getContext
  rd  <- guardJustM $ getField "redirect"
  doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
  res <-
    checkIDINEIDServiceTransactionForSignatory doc slid
    . EIDServiceAuthToView
    $ mkAuthKind doc
  mts <- case res of
    Just (est, ts, mctd) -> do
      nts <- updateIDINTransactionAfterCheck slid est ts mctd
      return $ Just nts
    _ -> return Nothing
  redirectPage <- renderTextTemplate "postIDINViewRedirect" $ do
    F.value "redirect" rd
    F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage

redirectEndpointFromIDINSignEIDServiceTransaction
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
redirectEndpointFromIDINSignEIDServiceTransaction did slid = do
  logInfo_ "EID Service signing transaction check"
  doc                 <- fst <$> getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad                  <- getAnalyticsData
  ctx                 <- getContext
  conf                <- eidServiceConf doc
  sessionID           <- getNonTempSessionID
  successOrInProgress <- do
    res <- checkIDINEIDServiceTransactionForSignatoryWithConf conf
                                                              slid
                                                              sessionID
                                                              EIDServiceAuthToSign
    case res of
      Just (_, EIDServiceTransactionStatusCompleteAndSuccess, Just _cd) -> return True
      -- the transaction cannot be just New or Started now,
      -- we were redirected here at the end of EID Auth
      _ -> return False
  let redirectUrl = "/s/" <> show did <> "/" <> show slid
  redirectPage <- renderTextTemplate "postIDINSignRedirect" $ do
    F.value "redirect" . B64.encode . BSC8.pack . urlEncode $ redirectUrl
    F.value "incorrect_data" $ not successOrInProgress
    F.value "document_id" $ show did
    F.value "signatory_link_id" $ show slid
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage
