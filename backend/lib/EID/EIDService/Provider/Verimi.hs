module EID.EIDService.Provider.Verimi (
    updateVerimiTransactionAfterCheck
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
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

updateVerimiTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransactionFromDB
  -> CompleteEIDServiceTransaction
  -> m EIDServiceTransactionStatus
updateVerimiTransactionAfterCheck slid est ct = do
  let ts = transactionStatus ct
  if estStatus est == ts
    then return $ estStatus est
    else do
      case (ts, ct) of
        (EIDServiceTransactionStatusCompleteAndSuccess, CompleteVerimiEIDServiceTransaction { completionDataVerimi = Just cd })
          -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let sl = fromJust $ getSigLinkFor slid doc
            if eidvtdVerifiedEmail cd == getEmail sl
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
                          eventFields
                          (Just sl)
                          Nothing
                    =<< signatoryActor ctx sl
                  chargeForItemSingle CIVerimiAuthentication $ documentid doc
                return EIDServiceTransactionStatusCompleteAndSuccess
              else do
                mergeEIDServiceTransactionWithStatus
                  EIDServiceTransactionStatusCompleteAndFailed
                return EIDServiceTransactionStatusCompleteAndFailed
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
