module EID.EIDService.Provider.NLIDIN (
    updateIDINTransactionAfterCheck
  ) where

import qualified Text.StringTemplates.Fields as F

import Chargeable
import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model.Query
import Doc.SignatoryLinkID
import EID.Authentication.Model
import EID.EIDService.Model
import EID.EIDService.Types
import EvidenceLog.Model
import Kontra hiding (InternalError)
import Session.Model
import Util.Actor
import Util.SignatoryLinkUtils

updateIDINTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransactionFromDB
  -> CompleteEIDServiceTransaction
  -> m EIDServiceTransactionStatus
updateIDINTransactionAfterCheck slid est ct = do
  let ts = transactionStatus ct
  if estStatus est == ts
    then return $ estStatus est
    else do
      case (ts, ct) of
        (EIDServiceTransactionStatusCompleteAndSuccess, CompleteNLIDINEIDServiceTransaction { completionDataNLIDIN = Just cd })
          -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let sl = fromJust $ getSigLinkFor slid doc
            mergeEIDServiceTransactionWithStatus
              EIDServiceTransactionStatusCompleteAndSuccess
            let auth = EIDServiceNLIDINAuthentication
                  { eidServiceIDINName          = eiditdName cd
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
                      eventFields
                      (Just sl)
                      Nothing
                =<< signatoryActor ctx sl
              chargeForItemSingle CIIDINAuthentication $ documentid doc
            return EIDServiceTransactionStatusCompleteAndSuccess
        (EIDServiceTransactionStatusCompleteAndSuccess, _) -> do
          mergeEIDServiceTransactionWithStatus
            EIDServiceTransactionStatusCompleteAndFailed
          return EIDServiceTransactionStatusCompleteAndFailed
        _ -> do
          mergeEIDServiceTransactionWithStatus ts
          return ts
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ est { estStatus = newstatus }
