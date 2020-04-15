module EID.EIDService.Provider.NOBankID (
    updateNOBankIDTransactionAfterCheck
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
import Doc.Model.Update
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
import Util.MonadUtils
import Util.SignatoryLinkUtils

updateNOBankIDTransactionAfterCheck
  :: Kontrakcja m
  => SignatoryLinkID
  -> EIDServiceTransactionFromDB
  -> CompleteEIDServiceTransaction
  -> m EIDServiceTransactionStatus
updateNOBankIDTransactionAfterCheck slid est ct = do
  let ts = transactionStatus ct
  if estStatus est == ts
    then return $ estStatus est
    else do
      case (ts, ct) of
        (EIDServiceTransactionStatusCompleteAndSuccess, CompleteNOBankIDEIDServiceTransaction { completionDataNOBankID = Just cd })
          -> do
            doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
            let signatoryLink = fromJust $ getSigLinkFor slid doc
            mergeEIDServiceTransactionWithStatus
              EIDServiceTransactionStatusCompleteAndSuccess
            let mDobFromEIDService = eidnobidBirthDate cd
                dobFromSignatoryLink =
                  resolveDateOfBirthFromSSN $ getPersonalNumber signatoryLink
                mNameFromEIDService = eidnobidName cd
            if isNothing mDobFromEIDService
              then do
                logAttention_ "Distinguished name not provided by EIDService."
                flashErrWithMessage
                  =<< renderTemplate_ "flashMessageNoDOBProvidedByEIDService"
                return EIDServiceTransactionStatusCompleteAndFailed
              else if isNothing mNameFromEIDService
                then do
                  logAttention_ "Name not provided by EIDService."
                  flashErrWithMessage
                    =<< renderTemplate_ "flashMessageNoNameProvidedByEIDService"
                  return EIDServiceTransactionStatusCompleteAndFailed
                else if mDobFromEIDService /= Just dobFromSignatoryLink
                  then do
                    logAttention
                        "Date of birth from EIDService does not match the one from SignatoryLink."
                      $ object
                          [ "dob_sl" .= dobFromSignatoryLink
                          , "dob_eidhub" .= mDobFromEIDService
                          , "provider" .= ("no_nobankid" :: Text)
                          ]
                    flashErrWithMessage =<< renderTemplate_
                      "flashMessageUserHasIdentifiedWithDifferentSSN"
                    return EIDServiceTransactionStatusCompleteAndFailed
                  else do
                    signatoryName <- guardJust $ eidnobidName cd
                    birthDate     <- guardJust mDobFromEIDService
                    let certificate      = decodeCertificate <$> eidnobidCertificate cd
                        phoneNumber      = eidnobidPhoneNumber cd
                        internalProvider = eidnobidInternalProvider cd
                        auth             = EIDServiceNOBankIDAuthentication
                          { eidServiceNOBankIDInternalProvider = internalProvider
                          , eidServiceNOBankIDSignatoryName    = signatoryName
                          , eidServiceNOBankIDPhoneNumber      = phoneNumber
                          , eidServiceNOBankIDDateOfBirth      = birthDate
                          , eidServiceNOBankIDCertificate      = certificate
                          }
                    sessionID <- getNonTempSessionID
                    dbUpdate $ MergeEIDServiceNOBankIDAuthentication (mkAuthKind doc)
                                                                     sessionID
                                                                     slid
                                                                     auth
                    ctx <- getContext
                    let pid         = eidnobidPid cd
                        signatoryDN = eidnobidDistinguishedName cd
                        issuerDN    = eidnobidIssuerDistinguishedName cd
                        eventFields = do
                          F.value "signatory_name" signatoryName
                          F.value "signatory_mobile" phoneNumber
                          F.value "provider_nobankid_eidservice" True
                          F.value "signatory_dob" birthDate
                          F.value "signatory_pid" pid
                          F.value "signatory_distinguished_name" signatoryDN
                          F.value "issuer_distinguished_name" issuerDN
                          F.value "signature" $ B64.encode <$> certificate
                    withDocument doc $ do
                      actor <- signatoryActor ctx signatoryLink
                      when (mkAuthKind doc == AuthenticationToView) $ do
                        void
                          . dbUpdate
                          . InsertEvidenceEventWithAffectedSignatoryAndMsg
                              AuthenticatedToViewEvidence
                              eventFields
                              (Just signatoryLink)
                              Nothing
                          $ actor

                      -- Updating phone number - mobile workflow only and only if not provided
                      forM_ phoneNumber $ \phone -> do
                        let
                          formattedPhoneFromEIDService = "+47" <> phone
                          slPhoneNumber                = getMobile signatoryLink
                          signatoryLinkIsEmpty         = slPhoneNumber == ""
                          formattedPhoneFromSignatory =
                            T.filter (\c -> c `notElem` (" -" :: String)) slPhoneNumber
                        when
                            (  not signatoryLinkIsEmpty
                            && formattedPhoneFromSignatory
                            /= formattedPhoneFromEIDService
                            )
                          $ do
                              logAttention_
                                "Not matching phone for NO BankID - should be blocked by EID provider"
                              internalError
                        when (signatoryLinkIsEmpty && Pending == documentstatus doc) $ do
                          dbUpdate $ UpdatePhoneAfterIdentificationToView
                            signatoryLink
                            phone
                            formattedPhoneFromEIDService
                            actor

                      chargeForItemSingle CINOBankIDAuthentication $ documentid doc
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
      either (unexpectedError "invalid base64 of NOBankID certificate") identity
        . B64.decode
        . T.encodeUtf8
    flashErrWithMessage :: Kontrakcja m => String -> m ()
    flashErrWithMessage = addFlashCookie . toCookieValue . toFlashMsg OperationFailed
    resolveDateOfBirthFromSSN :: Text -> Text
    resolveDateOfBirthFromSSN personalnumber =
      case T.chunksOf 2 (T.take 6 personalnumber) of
        [day, month, year] ->
          let yearWithoutCentury = read year
              sequenceNumber = read . T.take 3 . T.drop 6 $ personalnumber
              century = showt $ resolveCentury yearWithoutCentury sequenceNumber
          in  century <> year <> "-" <> month <> "-" <> day
        _ ->
          unexpectedError
            $  "This personal number cannot be formatted to date: "
            <> personalnumber
      where
        resolveCentury :: Int -> Int -> Int
        resolveCentury yearWithoutCentury sequenceNumber
          | yearWithoutCentury > 53 && sequenceNumber > 500 && sequenceNumber < 750 = 18
          | yearWithoutCentury > 40 && sequenceNumber > 899 = 19
          | yearWithoutCentury < 40 && sequenceNumber > 499 = 20
          | otherwise = 19
