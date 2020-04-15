module EID.EIDService.Provider.DKNemID (
    updateNemIDTransactionAfterCheck
  ) where

import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import FlashMessage
import Kontra hiding (InternalError)
import Session.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

updateNemIDTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransactionFromDB
  -> CompleteEIDServiceTransaction
  -> m EIDServiceTransactionStatus
updateNemIDTransactionAfterCheck slid est ct = do
  let ts = transactionStatus ct
  if estStatus est == ts
    then return $ estStatus est
    else do
      case (ts, ct) of
        (EIDServiceTransactionStatusCompleteAndSuccess, CompleteDKNemIDEIDServiceTransaction { completionDataDKNemID = Just cd })
          -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let signatoryLink = fromJust $ getSigLinkFor slid doc
            mergeEIDServiceTransactionWithStatus
              EIDServiceTransactionStatusCompleteAndSuccess
            let ssnFromEIDService    = normalizeSSN $ eidnidSSN cd
                ssnFromSignatoryLink = normalizeSSN $ getPersonalNumber signatoryLink
            if ssnFromEIDService /= ssnFromSignatoryLink
              then do
                logAttention "SSN from NETS does not match SSN from SignatoryLink."
                  $ object
                      [ "ssn_sl" .= ssnFromSignatoryLink
                      , "ssn_eidhub" .= ssnFromEIDService
                      , "provider" .= ("dk_nemid" :: Text)
                      ]
                flashMessageUserHasIdentifiedWithDifferentSSN
                  >>= addFlashCookie
                  .   toCookieValue
                return EIDServiceTransactionStatusCompleteAndFailed
              else do
                let signatoryName = cnFromDN $ eidnidDistinguishedName cd
                    birthDate     = eidnidBirthDate cd
                    certificate   = decodeCertificate $ eidnidCertificate cd
                    auth          = EIDServiceDKNemIDAuthentication
                      { eidServiceNemIDInternalProvider = eidnidInternalProvider cd
                      , eidServiceNemIDSignatoryName    = signatoryName
                      , eidServiceNemIDDateOfBirth      = birthDate
                      , eidServiceNemIDCertificate      = certificate
                      }
                sessionID <- getNonTempSessionID
                dbUpdate $ MergeEIDServiceNemIDAuthentication (mkAuthKind doc)
                                                              sessionID
                                                              slid
                                                              auth
                ctx <- getContext
                let pid         = eidnidPid cd
                    eventFields = do
                      F.value "signatory_name" signatoryName
                      F.value "provider_dknemid" True
                      F.value "signatory_dob" birthDate
                      F.value "signatory_pid" pid
                      F.value "signature" $ B64.encode certificate
                withDocument doc $ do
                  when (mkAuthKind doc == AuthenticationToView) $ do
                    void
                      $   dbUpdate
                      .   InsertEvidenceEventWithAffectedSignatoryAndMsg
                            AuthenticatedToViewEvidence
                            eventFields
                            (Just signatoryLink)
                            Nothing
                      =<< signatoryActor ctx signatoryLink
                  chargeForItemSingle CIDKNemIDAuthentication $ documentid doc
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
    decodeCertificate :: Text -> BSC8.ByteString
    decodeCertificate =
      either (unexpectedError "invalid base64 of NemID certificate") identity
        . B64.decode
        . T.encodeUtf8
    cnFromDN :: Text -> Text
    cnFromDN dn = fromMaybe parseError . lookup "CN" $ fmap
      parsePair
      (concatMap (T.splitOn " + ") $ T.splitOn ", " dn)
      where
        parsePair s = case T.splitOn "=" s of
          (name : values) -> (name, T.intercalate "=" values)
          _               -> unexpectedError $ "Cannot parse DN value: " <> dn
        parseError = unexpectedError $ "Cannot parse DN value: " <> dn
    normalizeSSN :: Text -> Text
    normalizeSSN = T.filter (/= '-')
    flashMessageUserHasIdentifiedWithDifferentSSN :: TemplatesMonad m => m FlashMessage
    flashMessageUserHasIdentifiedWithDifferentSSN = toFlashMsg OperationFailed
      <$> renderTemplate_ "flashMessageUserHasIdentifiedWithDifferentSSN"
